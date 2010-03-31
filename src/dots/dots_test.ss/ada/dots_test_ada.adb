-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2005-2009 (http://www.safirsdk.com)
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Unbounded; use  Ada.Strings.Wide_Unbounded;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Exceptions;
-- with GNAT.Spitbol; use GNAT.Spitbol;
with Safir.Dob.Typesystem;
with Safir.Dob.Typesystem.Channel_Id;
with Safir.Dob.Typesystem.Entity_Id;
with Safir.Dob.Typesystem.Handler_Id;
with Safir.Dob.Typesystem.Instance_Id;
with Safir.Dob.Typesystem.Members;
with Safir.Dob.Typesystem.Parameters;
--with Safir.Dob.Typesystem.Object;
with Safir.Dob.Typesystem.Operations; use Safir.Dob.Typesystem.Operations;
with Safir.Dob.Typesystem.Serialization;
with Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Library_Exceptions;
--
with Dots_Test.Test_Enum;
with Dots_Test.Test_Exception;
with Dots_Test.Arrays_Item;
with Dots_Test.Member_Items_Array;
with Dots_Test.Member_Items;
with Dots_Test.Member_Types_Base;
with Dots_Test.Member_Types;
with Dots_Test.Member_Types_Property;
with Dots_Test.Parameter_Types;
with Dots_Test.Member_Arrays;
with Dots_Test.Member_Arrays_Property;
with Dots_Test.Parameter_Arrays;
with Dots_Test.Types_Item;
with Dots_Test.Empty_Object;

-- AWI: test
--  with Awi_Test.My_Entity;
--  with Awi_Test.My_Property;
--  with Test.Parent;
--  with Test.Child;
--  with Test.Grand_Child;
--  with Test.Another_Child;
--  with Safir.Dob.Typesystem.Object;
with Dots_Test.Test_Item;
with Safir.Dob.Typesystem.Container_Instantiations;
with Ada.Strings.Wide_Fixed;

pragma Warnings ("D");  -- turn off warnings for implicit dereference
pragma Warnings ("U");

procedure Dots_Test_Ada is

   use type Safir.Dob.Typesystem.Int_32;

   function Trim (Source : in String;
                  Side   : in Ada.Strings.Trim_End := Ada.Strings.Both) return String
                  renames Ada.Strings.Fixed.Trim;

   function Trim (Source : in Wide_String;
                  Side   : in Ada.Strings.Trim_End := Ada.Strings.Both)
                 return Wide_String
      renames Ada.Strings.Wide_Fixed.Trim;

   function Trim (Source : in Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
                  Side   : in Ada.Strings.Trim_End := Ada.Strings.Both)
                 return Ada.Strings.Wide_Unbounded.Unbounded_Wide_String is
   begin
      return Ada.Strings.Wide_Unbounded.Trim (Source, Side);
   end Trim;


   function Trim_Float_String (S : in String) return String is
      Tmp : String := S;
   begin
      if Tmp (Tmp'Last - 2 .. Tmp'Last) = ".00" then
         Tmp (Tmp'Last) := ' ';
         Tmp (Tmp'Last - 1) := ' ';
         Tmp (Tmp'Last - 2) := ' ';
      end if;
      return Trim (Tmp);
   end Trim_Float_String;

   package Float_32_Io is new Ada.Text_IO.Float_IO (Safir.Dob.Typesystem.Float_32);
   package Float_64_Io is new Ada.Text_IO.Float_IO (Safir.Dob.Typesystem.Float_64);



--
--     package PT renames DotsTest.ParameterTypes;
--     package PA renames  DotsTest.ParameterArrays;
--     package MT renames Dots_Test.Member_Types;
--     package MA renames  Dots_Test.Member_Arrays;
--     package MTP renames DotsTest.MemberTypesProperty;
--     package MAP renames  DotsTest.MemberArraysProperty;
--
   MA1_Smart_Ptr, MA2_Smart_Ptr : Dots_Test.Member_Arrays.Smart_Pointer;
   MA1_Ptr, MA2_Ptr : Dots_Test.Member_Arrays.Member_Arrays_Class_Access;

   MT1_Smart_Ptr, MT2_Smart_Ptr : Dots_Test.Member_Types.Smart_Pointer;
   MT1_Ptr, MT2_Ptr : Dots_Test.Member_Types.Member_Types_Class_Access;

   MI_Smart_Ptr : Dots_Test.Member_Items.Smart_Pointer;
   MI_Ptr : Dots_Test.Member_Items.Member_Items_Class_Access;

   MIA_Smart_Ptr : Dots_Test.Member_Items_Array.Smart_Pointer;
   MIA_Ptr : Dots_Test.Member_Items_Array.Member_Items_Array_Class_Access;

   EO_Smart_Ptr : Dots_Test.Empty_Object.Smart_Pointer;
   --EO_Ptr : Dots_Test.Empty_Object.Empty_Object_Class_Access;

   function Strip (Str : in String; Ch  : in Character) return String is
      US : Unbounded_String;
   begin
      for Idx in Str'Range loop
         if Str (Idx) /= Ch then
            Append (US, Str (Idx));
         end if;
      end loop;
      return To_String (US);
   end Strip;

   function To_Lower (Str : in String) return String is
   begin
      return Ada.Strings.Fixed.Translate (Str,
        Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Lower;


   procedure Header (Label : in String) is
   begin
      New_Line (3);
      Put_Line ("=====================================================");
      Put_Line ("Testing: " & Label);
      Put_Line ("=====================================================");
   end Header;

   procedure Print (Label : in String) is
   begin
      Put_Line (Label);
   end Print;

   procedure Print (Label : in String;
                    Val   : in String) is
   begin
      Put_Line (Label & Val);
   end Print;

   procedure Print (Label : in String;
                    Val   : in Integer) is
   begin
      Print (Label, Strip (Integer'Image (Val), ' '));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Unbounded_Wide_String) is
   begin
      Print (Label, Safir.Dob.Typesystem.Utilities.To_Utf_8 (Val));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Int_32) is
   begin
      Print (Label, Strip (Safir.Dob.Typesystem.Int_32'Image (Val), ' '));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Int_64) is
   begin
      Print (Label, Trim (Safir.Dob.Typesystem.Int_64'Image (Val)));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Float_32) is
      S : String (1 .. 20);
   begin
      Float_32_Io.Put (To => S, Item => Val, Aft => 2, Exp => 0);
      Print (Label, Trim_Float_String (S));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Float_64) is
      S : String (1 .. 20);
   begin
      Float_64_Io.Put (To => S, Item => Val, Aft => 2, Exp => 0);
      Print (Label, Trim_Float_String (S));
   end Print;

--     procedure Print (Label : in String;
--                      Val   : in Safir.Dob.Typesystem.MemberIndex) is
--     begin
--        Print (Label, Safir.Dob.Typesystem.MemberIndex'Image (Val));
--     end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type) is
   begin
      Print (Label, Trim (Safir.Dob.Typesystem.Instance_Id.To_String (Val)));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type) is
   begin
      Print (Label, Safir.Dob.Typesystem.Entity_Id.To_String (Val));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type) is
   begin
      Print (Label, Trim (Safir.Dob.Typesystem.Channel_Id.To_String (Val)));
   end Print;

   procedure Print (Label : in String;
                    Val   : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type) is
   begin
      Print (Label, Trim (Safir.Dob.Typesystem.Handler_Id.To_String (Val)));
   end Print;

   procedure Print_Type_Id (Label : in String;
                            Val   : in Safir.Dob.Typesystem.Type_Id) is
   begin
      Print (Label, Safir.Dob.Typesystem.Utilities.To_Utf_8 (Safir.Dob.Typesystem.Operations.Get_Name (Val)));
   end Print_Type_Id;

   procedure Print (Label : in String;
                    Val   : in Boolean) is
   begin
      Print (Label, To_Lower (Boolean'Image (Val)));
   end Print;


--     procedure Print (Label : in String;
--                      Val   : in Safir.Dob.Typesystem.Object.Class'Class) is
--     begin
--        Print (Label, Safir.Dob.Typesystem.Serialization.ToXml (Val));
--     end Print;
--
   procedure Test_Has_Property is
   begin
      Header ("Has Property");
      Print ("MemberTypes - MemberTypesProperty: ",
             Dots_Test.Member_Types_Property.Has_Property (MT1_Smart_Ptr));
      Print ("MemberTypes - MemberArraysProperty: ",
             Dots_Test.Member_Arrays_Property.Has_Property (MT1_Smart_Ptr));
      Print ("MemberArrays - MemberTypesProperty: ",
             Dots_Test.Member_Types_Property.Has_Property (MA1_Smart_Ptr));
      Print ("MemberArrays - MemberArraysProperty: ",
             Dots_Test.Member_Arrays_Property.Has_Property (MA1_Smart_Ptr));
      Print ("MemberItems - MemberTypesProperty: ",
             Dots_Test.Member_Types_Property.Has_Property (MI_Smart_Ptr));
      Print ("MemberItems - MemberArraysProperty: ",
             Dots_Test.Member_Arrays_Property.Has_Property (MI_Smart_Ptr));
      Print ("MemberItemsArray - MemberTypesProperty: ",
             Dots_Test.Member_Types_Property.Has_Property (MIA_Smart_Ptr));
      Print ("MemberItemsArray - MemberArraysProperty: ",
             Dots_Test.Member_Arrays_Property.Has_Property (MIA_Smart_Ptr));
      Print ("EmptyObject - MemberTypesProperty: ",
             Dots_Test.Member_Types_Property.Has_Property (EO_Smart_Ptr));
      Print ("EmptyObject - MemberArraysProperty: ",
             Dots_Test.Member_Arrays_Property.Has_Property (EO_Smart_Ptr));
   end Test_Has_Property;

   procedure Test_Get_Name is
   begin
      Header ("Get Name");
      Print ("MemberTypes          - ",
             Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Types.Class_Type_Id));
      Print ("MemberArrays         - ",
             Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Arrays.Class_Type_Id));
      Print ("MemberTypesProperty  - ",
             Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Types_Property.Class_Type_Id));
      Print ("MemberArraysProperty - ",
             Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Arrays_Property.Class_Type_Id));
      Print ("MemberItems          - ",
             Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Items.Class_Type_Id));
      Print ("MemberItemsArray     - ",
             Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Items_Array.Class_Type_Id));
      Print ("EmptyObject          - ",
             Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Empty_Object.Class_Type_Id));
   end Test_Get_Name;

   procedure Test_Get_Number_Of_Members is
   begin
      Header ("Get Number Of Members");
      Print ("MemberTypes          - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Member_Types.Class_Type_Id));
      Print ("MemberArrays         - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Member_Arrays.Class_Type_Id));
      Print ("MemberTypesProperty  - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Member_Types_Property.Class_Type_Id));
      Print ("MemberArraysProperty - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Member_Arrays_Property.Class_Type_Id));
      Print ("MemberItems          - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Member_Items.Class_Type_Id));
      Print ("MemberItemsArray     - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Member_Items_Array.Class_Type_Id));
      Print ("EmptyObject          - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Empty_Object.Class_Type_Id));
      Print ("ParameterTypes       - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Parameter_Types.Class_Type_Id));
      Print ("ParameterArrays      - ",
             Safir.Dob.Typesystem.Members.Get_Number_Of_Members (Dots_Test.Parameter_Arrays.Class_Type_Id));
   end Test_Get_Number_Of_Members;

   procedure Test_Get_Number_Of_Parameters is
   begin
      Header ("Get Number Of Parameters");
      Print ("MemberTypes          - ",
             Safir.Dob.Typesystem.Parameters.Get_Number_Of_Parameters (Dots_Test.Member_Types.Class_Type_Id));
      Print ("MemberArrays         - ",
             Safir.Dob.Typesystem.Parameters.Get_Number_Of_Parameters (Dots_Test.Member_Arrays.Class_Type_Id));
      Print ("MemberItems          - ",
             Safir.Dob.Typesystem.Parameters.Get_Number_Of_Parameters (Dots_Test.Member_Items.Class_Type_Id));
      Print ("MemberItemsArray     - ",
             Safir.Dob.Typesystem.Parameters.Get_Number_Of_Parameters (Dots_Test.Member_Items_Array.Class_Type_Id));
      Print ("EmptyObject          - ",
             Safir.Dob.Typesystem.Parameters.Get_Number_Of_Parameters (Dots_Test.Empty_Object.Class_Type_Id));
      Print ("ParameterTypes       - ",
             Safir.Dob.Typesystem.Parameters.Get_Number_Of_Parameters (Dots_Test.Parameter_Types.Class_Type_Id));
      Print ("ParameterArrays      - ",
             Safir.Dob.Typesystem.Parameters.Get_Number_Of_Parameters (Dots_Test.Parameter_Arrays.Class_Type_Id));
   end Test_Get_Number_Of_Parameters;


   procedure Test_Create_Routines is
   begin
      Header ("Create routines (Types)");
      Print ("Create_ParameterTypes: ",
        Safir.Dob.Typesystem.Serialization.To_Xml
          (Dots_Test.Member_Types.Create_Parameter_Types
             (Dots_Test.Parameter_Types.Int_32_Parameter,
              Dots_Test.Parameter_Types.Enumeration_Parameter,
              Dots_Test.Parameter_Types.Test_Class_Parameter)));
      Print ("CreateValueTypes     : ",
             Safir.Dob.Typesystem.Serialization.To_Xml
              (Dots_Test.Member_Types.Create_Value_Types));
      Print ("Create_ValueArrays   : ",
             Safir.Dob.Typesystem.Serialization.To_Xml
               (Dots_Test.Member_Types.Create_Value_Arrays));

      Header ("Create routines (Arrays)");
      Print ("Create_ParameterArrays: ",
        Safir.Dob.Typesystem.Serialization.To_Xml
          (Dots_Test.Member_Arrays.Create_Parameter_Arrays
             (Dots_Test.Parameter_Types.Int_32_Parameter,
              Dots_Test.Parameter_Types.Enumeration_Parameter,
              Dots_Test.Parameter_Types.Test_Class_Parameter)));
      Print ("Create_ValueArraysOne : ",
             Safir.Dob.Typesystem.Serialization.To_Xml
               (Dots_Test.Member_Arrays.Create_Value_Arrays_One));
      Print ("Create_ValueArraysAll : ",
             Safir.Dob.Typesystem.Serialization.To_Xml
               (Dots_Test.Member_Arrays.Create_Value_Arrays_All));
   end Test_Create_Routines;


   Failed_Tests    : Natural := 0;
   Performed_Tests : Natural := 0;

   procedure Check (Expr        : in Boolean;
                    Description : in String := "") is
   begin
      Performed_Tests := Performed_Tests + 1;

      if not Expr then
         Put ("!! Check" & Natural'Image (Performed_Tests));
         if Description /= "" then
            Put (" (" & Description & ")");
         end if;
         Put_Line (" failed");
         Failed_Tests := Failed_Tests + 1;
      else
         null;
         Put ("Check" & Natural'Image (Performed_Tests));
         if Description /= "" then
            Put (" (" & Description & ")");
         end if;
         Put_Line (" succeded");
      end if;
   end Check;

--     function Get_Blob_Size (Obj : Safir.Dob.Typesystem.Object.Object_Type'Class)
--                                   return Safir.Dob.Typesystem.Int_32 is
--     begin
--        return Obj.Calculate_Blob_Size;
--     end Get_Blob_Size;

--     procedure Test_Class is
--        Parent : Test.Parent.Parent_Type;
--        Child  : Test.Child.Child_Type;
--        Grand_Child : Test.Grand_Child.Grand_Child_Type;
--
--        Parent_Class : Test.Parent.Parent_Type'Class := Parent;
--        Child_Class : Test.Parent.Parent_Type'Class := Child;
--        Grand_Child_Class : Test.Parent.Parent_Type'Class := Grand_Child;
--
--        C_Class     : Test.Child.Child_Type'Class := Child;
--        GC_Class     : Test.Grand_Child.Grand_Child_Type'Class := Grand_Child;
--
--
--        procedure Foo (Bar : Test.Parent.Parent_Type'Class) is
--        begin
--           Print ("Bollocks");
--        end;
--
--     begin
--        Print ("Parent_Class size (bits): ", Integer'Image (Parent_Class'Size));
--        Print ("Child_Class size (bits): ", Integer'Image (Child_Class'Size));
--        Print ("Grand_Child_Class size (bits): ", Integer'Image (Grand_Child_Class'Size));
--        Foo (Parent);
--        Foo (Child);
--        Foo (Grand_Child);
--        Foo (Parent_Class);
--        Foo (Child_Class);
--        Foo (Grand_Child_Class);
--        Foo (C_Class);
--        Foo (GC_Class);
--  --      Parent_Class := Test.Parent.Parent_Type'Class (Parent);
--  --      Parent_Class := Test.Parent.Parent_Type'Class (Child);
--
--        Child_Class := Test.Parent.Parent_Type'Class (Parent);
--
--        --Grand_Child_Class := Grand_Child;
--
--        --Parent_Class := Grand_Child_Class;
--
--     end Test_Class;

--     procedure Test_Poly is
--
--        Parent_Ptr               : Test.Parent.Smart_Pointer := Test.Parent.Create;
--        Child_Ptr                : Test.Child.Smart_Pointer := Test.Child.Create;
--        Grand_Child_Ptr          : Test.Grand_Child.Smart_Pointer := Test.Grand_Child.Create;
--        Another_Child_Ptr        : Test.Another_Child.Smart_Pointer := Test.Another_Child.Create;
--
--        Parent_Access : Test.Parent.Parent_Class_Access := Parent_Ptr.Ref;
--        Child_Access : Test.Child.Child_Class_Access := Child_Ptr.Ref;
--        Grand_Child_Access : Test.Grand_Child.Grand_Child_Class_Access := Grand_Child_Ptr.Ref;
--        Another_Child_Access : Test.Another_Child.Another_Child_Class_Access := Another_Child_Ptr.Ref;
--
--        use type Safir.Dob.Typesystem.Int_64;
--
--     begin
--        Check (Parent_Access.Get_Type_Id = 2407368795643760510);
--        Check (Child_Access.Get_Type_Id = 3398855741022430189);
--
--        Check (Get_Blob_Size (Parent_Access.all) = 21);
--        Check (Get_Blob_Size (Child_Access.all) = 30);
--        Check (Get_Blob_Size (Grand_Child_Access.all) = 39);
--
--        Check (Parent_Ptr.Use_Count = 1);
--        Check (Child_Ptr.Use_Count = 1);
--        Check (Grand_Child_Ptr.Use_Count = 1);
--
--        -- Parent doesn't point to a child so a down-cast should give Constraint_Error
--        declare
--        begin
--           Child_Ptr := Test.Child.Convert (Parent_Ptr);
--           Check (False);
--        exception
--           when Constraint_Error =>
--              Check (True);
--        end;
--
--        Parent_Access.Parent_Int.Set_Val (99);
--        Check (Parent_Access.Parent_Int.Get_Val = 99, "XX");
--
--        Grand_Child_Access.Parent_Int.Set_Val (55);
--
--        -- Convert up
--        Parent_Ptr := Test.Parent.Convert (Grand_Child_Ptr);
--        Parent_Access := Parent_Ptr.Ref;
--
--        Check (Parent_Access.Parent_Int.Get_Val = 55);
--        Check (Get_Blob_Size (Parent_Access.all) = 39);
--
--        Print ("Use count: ", Natural'Image (Parent_Ptr.Use_Count));
--
--        Check (Parent_Ptr.Use_Count = 2);
--        Check (Grand_Child_Ptr.Use_Count = 2);
--
--        declare
--           -- Convert down
--           CP : Test.Child.Smart_Pointer := Test.Child.Convert (Parent_Ptr);
--        begin
--           Check (Parent_Ptr.Use_Count = 3);
--           Check (CP.Use_Count = 3);
--        end;
--        Check (Parent_Ptr.Use_Count = 2);
--
--        -- Convert down
--        Child_Ptr := Test.Child.Convert (Parent_Ptr);
--        Child_Access := Child_Ptr.Ref;
--        Check (Get_Blob_Size (Child_Access.all) = 39);
--
--        Parent_Ptr := Test.Parent.Convert (Grand_Child_Ptr);
--        Parent_Access := Parent_Ptr.Ref;
--        --Check (Parent_Access.Get_Type_Id = -6626636272276630930);
--
--     end Test_Poly;

--     procedure Test_Prop is
--        E : Awi_Test.My_Entity.Smart_Pointer := Awi_Test.My_Entity.Create;
--        I : Dots_Test.Test_Item.Smart_Pointer := Dots_Test.Test_Item.Create;
--     begin
--        Header ("Prop test");
--        E.Ref.My_Item.Set_Ptr (I);
--
--        Awi_Test.My_Property.Set_Member_1 (E, 55);
--        Print ("Set_member 1 ok");
--
--        Awi_Test.My_Property.Set_Member_2 (E, 99);
--        Print ("Set_member 2 ok");
--
--        if Awi_Test.My_Property.Get_Member_1 (E) = 55 then
--           Print ("Get Member 1 ok");
--        else
--           Print ("Get Member 1 NOT ok");
--        end if;
--
--        if Awi_Test.My_Property.Get_Member_2 (E) = 99 then
--           Print ("Get Member 2 ok");
--        else
--           Print ("Get Member 2 NOT ok");
--        end if;
--
--
--     end Test_Prop;

   procedure Test_Int_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Int32");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Int_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Int_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Int_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Int_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Int_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Int_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Int_32_Member.Is_Changed;
      MT1_Ptr.Int_32_Member.Set_Val (Dots_Test.Parameter_Types.Int_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Int_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Int_32_Member.Is_Changed;

      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Int_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Int_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Int_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Int32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Int32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Int32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Int_32_Member (MT2_Smart_Ptr, MT1_Ptr.Int_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Int_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Int_32_Member (MI_Smart_Ptr, MT2_Ptr.Int_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Int_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Int_32_Member (MIA_Smart_Ptr, MT2_Ptr.Int_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Int_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Int_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Int_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Int_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Int_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Int_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Int_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Int_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Int_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Int_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Int_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Int_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Int_32_Member.Element (Ix).Set_Val (MA1_Ptr.Int_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Int_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Int_32_Member.Element (Ix).Set_Val (MA1_Ptr.Int_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Int_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Int_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Int_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Int_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Int_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Int_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Int_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Int_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Int_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Int_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Int_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Int_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Int_32;

   procedure Test_Int_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Int64");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Int_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Int_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Int_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Int_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Int_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Int_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Int_64_Member.Is_Changed;
      MT1_Ptr.Int_64_Member.Set_Val (Dots_Test.Parameter_Types.Int_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Int_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Int_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Int_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Int_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Int_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Int64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Int64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Int64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Int_64_Member (MT2_Smart_Ptr, MT1_Ptr.Int_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Int_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Int_64_Member (MI_Smart_Ptr, MT2_Ptr.Int_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Int_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Int_64_Member (MIA_Smart_Ptr, MT2_Ptr.Int_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Int_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Int_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Int_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Int_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Int_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Int_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Int_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Int_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Int_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Int_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Int_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Int_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Int_64_Member.Element (Ix).Set_Val (MA1_Ptr.Int_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Int_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Int_64_Member.Element (Ix).Set_Val (MA1_Ptr.Int_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Int_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Int_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Int_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Int_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Int_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Int_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Int_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Int_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Int_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Int_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Int_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Int_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Int_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Int_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Int_64;

   procedure Test_Float_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Float32");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Float_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Float_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Float_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Float_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Float_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Float_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Float_32_Member.Is_Changed;
      MT1_Ptr.Float_32_Member.Set_Val (Dots_Test.Parameter_Types.Float_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Float_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Float_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Float_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Float_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Float_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Float32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Float32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Float32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Float_32_Member (MT2_Smart_Ptr, MT1_Ptr.Float_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Float_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Float_32_Member (MI_Smart_Ptr, MT2_Ptr.Float_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Float_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Float_32_Member (MIA_Smart_Ptr, MT2_Ptr.Float_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Float_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Float_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Float_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Float_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Float_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Float_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Float_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Float_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Float_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Float_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Float_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Float_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Float_32_Member.Element (Ix).Set_Val (MA1_Ptr.Float_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Float_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Float_32_Member.Element (Ix).Set_Val (MA1_Ptr.Float_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Float_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Float_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Float_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Float_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Float_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Float_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Float_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Float_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Float_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Float_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Float_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Float_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Float_32;

   procedure Test_Float_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Float64");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Float_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Float_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Float_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Float_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Float_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Float_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Float_64_Member.Is_Changed;
      MT1_Ptr.Float_64_Member.Set_Val (Dots_Test.Parameter_Types.Float_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Float_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Float_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Float_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Float_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Float_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Float64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Float64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Float64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Float_64_Member (MT2_Smart_Ptr, MT1_Ptr.Float_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Float_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Float_64_Member (MI_Smart_Ptr, MT2_Ptr.Float_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Float_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Float_64_Member (MIA_Smart_Ptr, MT2_Ptr.Float_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Float_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Float_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Float_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Float_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Float_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Float_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Float_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Float_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Float_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Float_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Float_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Float_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Float_64_Member.Element (Ix).Set_Val (MA1_Ptr.Float_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Float_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Float_64_Member.Element (Ix).Set_Val (MA1_Ptr.Float_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Float_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Float_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Float_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Float_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Float_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Float_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Float_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Float_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Float_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Float_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Float_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Float_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Float_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Float_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Float_64;

   procedure Test_Boolean is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Boolean");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Boolean_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Boolean_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Boolean_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Boolean_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Boolean_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Boolean_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Boolean_Member.Is_Changed;
      MT1_Ptr.Boolean_Member.Set_Val (Dots_Test.Parameter_Types.Boolean_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Boolean_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Boolean_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Boolean_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Boolean_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Boolean_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("BooleanParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("BooleanParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("BooleanParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Boolean_Member (MT2_Smart_Ptr, MT1_Ptr.Boolean_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Boolean_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Boolean_Member (MI_Smart_Ptr, MT2_Ptr.Boolean_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Boolean_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Boolean_Member (MIA_Smart_Ptr, MT2_Ptr.Boolean_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Boolean_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Boolean_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Boolean_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Boolean_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Boolean_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Boolean_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Boolean_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Boolean_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Boolean_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Boolean_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Boolean_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Boolean_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Boolean_Member.Element (Ix).Set_Val (MA1_Ptr.Boolean_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Boolean_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Boolean_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Boolean_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Boolean_Member.Element (Ix).Set_Val (MA1_Ptr.Boolean_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Boolean_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Boolean_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Boolean_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Boolean_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Boolean_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Boolean_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Boolean_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Boolean_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Boolean_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Boolean_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Boolean_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Boolean_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Boolean_Member (MT2_Smart_Ptr);
      MA1_Ptr.Boolean_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Boolean_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Boolean_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Boolean_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Boolean_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Boolean_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Boolean;

   procedure Test_Enumeration is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Enumeration");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Enumeration_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Enumeration_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Enumeration_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Enumeration_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Enumeration_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Enumeration_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Enumeration_Member.Is_Changed;
      MT1_Ptr.Enumeration_Member.Set_Val (Dots_Test.Parameter_Types.Enumeration_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Enumeration_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Enumeration_Member.Is_Changed;
      Print ("First: ", Integer (Dots_Test.Test_Enum.Enumeration'Pos
        (Dots_Test.Test_Enum.Enumeration'First)));
      Print ("Last: ", Integer (Dots_Test.Test_Enum.Enumeration'Pos
        (Dots_Test.Test_Enum.Enumeration'Last)));
      Print ("Size: ", Integer (Dots_Test.Test_Enum.Enumeration'Pos
        (Dots_Test.Test_Enum.Enumeration'Last)) + 1);
      Print ("Test ToString (0): ", Dots_Test.Test_Enum.To_Dou_String
             (Dots_Test.Test_Enum.My_First));
      Print ("Test ToValue (MySecond): ", Integer (Dots_Test.Test_Enum.Enumeration'Pos
          (Dots_Test.Test_Enum.From_Dou_String (Dots_Test.Test_Enum.To_Dou_String
            (Dots_Test.Test_Enum.My_Second)))));

      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Enumeration_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Enumeration_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Enumeration_Member_Member_Index));
      Print ("GetTypeId: ", Safir.Dob.Typesystem.Members.Get_Type_Id (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Enumeration_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("EnumerationParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("EnumerationParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("EnumerationParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Enumeration_Member (MT2_Smart_Ptr, MT1_Ptr.Enumeration_Member.Get_Val);
      Print ("Val: ", Dots_Test.Test_Enum.To_Dou_String (Dots_Test.Member_Types_Property.Get_Enumeration_Member (MT2_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Enumeration_Member (MI_Smart_Ptr, MT2_Ptr.Enumeration_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Test_Enum.To_Dou_String (Dots_Test.Member_Types_Property.Get_Enumeration_Member (MI_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Enumeration_Member (MIA_Smart_Ptr, MT2_Ptr.Enumeration_Member.Get_Val);
      Print ("Item Array Val: ", Dots_Test.Test_Enum.To_Dou_String (MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Enumeration_Member.Get_Val));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Test_Enum.To_Dou_String (Dots_Test.Member_Types_Property.Get_Enumeration_Member (EO_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Enumeration_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Enumeration_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Enumeration_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Enumeration_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Enumeration_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Enumeration_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Enumeration_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Enumeration_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Enumeration_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Enumeration_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Enumeration_Member.Element (Ix).Set_Val (MA1_Ptr.Enumeration_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Enumeration_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Enumeration_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ",
                Dots_Test.Test_Enum.To_Dou_String (Dots_Test.Member_Arrays_Property.Get_Enumeration_Member (MA2_Smart_Ptr, Ix)));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Enumeration_Member.Element (Ix).Set_Val (MA1_Ptr.Enumeration_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ",
                Dots_Test.Test_Enum.To_Dou_String (Dots_Test.Member_Arrays_Property.Get_Enumeration_Member (MI_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Enumeration_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Enumeration_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ",
                Dots_Test.Test_Enum.To_Dou_String (Dots_Test.Member_Arrays_Property.Get_Enumeration_Member (MIA_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Enumeration_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Enumeration_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Enumeration_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Enumeration_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ",
                Dots_Test.Test_Enum.To_Dou_String (Dots_Test.Member_Arrays_Property.Get_Enumeration_Member (EO_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Enumeration_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Enumeration_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Enumeration_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Enumeration_Member (MT2_Smart_Ptr);
      MA1_Ptr.Enumeration_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Enumeration_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Enumeration_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Enumeration_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Enumeration_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Enumeration_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Enumeration;

   function Check_String (Str : Unbounded_Wide_String; Idx : Safir.Dob.Typesystem.Array_Index) return String is
      Safir_Str     : constant Unbounded_Wide_String :=  To_Unbounded_Wide_String ("Safir" & Wide_Character'Val (16#Ae#));
      Safir_Str_Rev : constant Unbounded_Wide_String :=  To_Unbounded_Wide_String (Wide_Character'Val (16#Ae#) & "rifaS");
      Correct       : Boolean := False;
   begin
      if Idx = 0 then
         if Str = Safir_Str then
            Correct := True;
         end if;
      end if;
      if Idx = 1 then
         if Str = Safir_Str_Rev then
            Correct := True;
         end if;
      end if;

      if Correct then
         return "<correct>";
      else
         return "<INCORRECT!>";
      end if;
   end Check_String;

   function Check_String (Str : Unbounded_Wide_String) return String is
   begin
      return Check_String (Str, 0);
   end Check_String;

   procedure Test_String is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("String");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.String_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.String_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.String_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.String_Member_Array_Size = 2 and
                 Dots_Test.Member_Arrays_Property.String_Member_Array_Size (MA1_Smart_Ptr) = 2);
      Print ("MaxStringLength Size Ok (10): ",
             Dots_Test.Member_Types_Base.String_Member_Max_String_Length = 10 and
               Dots_Test.Member_Types_Base.String_Member_Max_String_Length = 10);
      Null_Ok := MT1_Ptr.String_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.String_Member.Is_Changed;
      MT1_Ptr.String_Member.Set_Val (Dots_Test.Parameter_Types.String_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.String_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.String_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.String_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.String_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.String_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("StringParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("StringParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("StringParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_String_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_String_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_String_Member (MT2_Smart_Ptr, MT1_Ptr.String_Member.Get_Val);
      Print ("Val: ", Check_String (Dots_Test.Member_Types_Property.Get_String_Member (MT2_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_String_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_String_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_String_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_String_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_String_Member (MI_Smart_Ptr, MT2_Ptr.String_Member.Get_Val);
      Print ("Item Val: ", Check_String (Dots_Test.Member_Types_Property.Get_String_Member (MI_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_String_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_String_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_String_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_String_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_String_Member (MIA_Smart_Ptr, MT2_Ptr.String_Member.Get_Val);
      Print ("Item Array Val: ", Check_String (MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.String_Member.Get_Val));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_String_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_String_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_String_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_String_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_String_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_String_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_String_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.String_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.String_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.String_Member.Element (Ix).Is_Changed;
         MA1_Ptr.String_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.String_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.String_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.String_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_String_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_String_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.String_Member.Element (Ix).Set_Val (MA1_Ptr.String_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_String_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_String_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Check_String (Dots_Test.Member_Arrays_Property.Get_String_Member (MA2_Smart_Ptr, Ix), Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.String_Member.Element (Ix).Set_Val (MA1_Ptr.String_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Check_String (Dots_Test.Member_Arrays_Property.Get_String_Member (MI_Smart_Ptr, Ix), Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_String_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_String_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Check_String (Dots_Test.Member_Arrays_Property.Get_String_Member (MIA_Smart_Ptr, Ix), Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_String_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_String_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_String_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_String_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_String_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_String_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_String_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.String_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_String_Member (MT2_Smart_Ptr);
      MA1_Ptr.String_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_String_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.String_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_String_Member (MT2_Smart_Ptr) and
        MA1_Ptr.String_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_String_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_String;

   procedure Test_Entity_Id is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("EntityId");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Entity_Id_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Entity_Id_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Entity_Id_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Entity_Id_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Entity_Id_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Entity_Id_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Entity_Id_Member.Is_Changed;
      MT1_Ptr.Entity_Id_Member.Set_Val (Dots_Test.Parameter_Types.Entity_Id_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Entity_Id_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Entity_Id_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Entity_Id_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Entity_Id_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Entity_Id_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("EntityIdParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("EntityIdParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("EntityIdParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Entity_Id_Member (MT2_Smart_Ptr, MT1_Ptr.Entity_Id_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Entity_Id_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Entity_Id_Member (MI_Smart_Ptr, MT2_Ptr.Entity_Id_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Entity_Id_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Entity_Id_Member (MIA_Smart_Ptr, MT2_Ptr.Entity_Id_Member.Get_Val);
      Print ("Item Array Val:", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Entity_Id_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Entity_Id_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Entity_Id_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Entity_Id_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Entity_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Entity_Id_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Entity_Id_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Entity_Id_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Entity_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Entity_Id_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Entity_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Entity_Id_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Entity_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Entity_Id_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Entity_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Entity_Id_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Entity_Id_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Entity_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Entity_Id_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Entity_Id_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Entity_Id_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Entity_Id_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Entity_Id_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Entity_Id_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Entity_Id_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Entity_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Entity_Id_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Entity_Id_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Entity_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Entity_Id_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Entity_Id_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Entity_Id_Member (MT2_Smart_Ptr);
      MA1_Ptr.Entity_Id_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Entity_Id_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Entity_Id_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Entity_Id_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Entity_Id_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Entity_Id_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Entity_Id;

   procedure Test_Instance_Id is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("InstanceId");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Instance_Id_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Instance_Id_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Instance_Id_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Instance_Id_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Instance_Id_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Instance_Id_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Instance_Id_Member.Is_Changed;
      MT1_Ptr.Instance_Id_Member.Set_Val (Dots_Test.Parameter_Types.Instance_Id_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Instance_Id_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Instance_Id_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Instance_Id_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Instance_Id_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Instance_Id_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("InstanceIdParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("InstanceIdParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("InstanceIdParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Instance_Id_Member (MT2_Smart_Ptr, MT1_Ptr.Instance_Id_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Instance_Id_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Instance_Id_Member (MI_Smart_Ptr, MT2_Ptr.Instance_Id_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Instance_Id_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Instance_Id_Member (MIA_Smart_Ptr, MT2_Ptr.Instance_Id_Member.Get_Val);
      Print ("Item Array Val:", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Instance_Id_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Instance_Id_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Instance_Id_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Instance_Id_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Instance_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Instance_Id_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Instance_Id_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Instance_Id_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Instance_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Instance_Id_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Instance_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Instance_Id_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Instance_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Instance_Id_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Instance_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Instance_Id_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Instance_Id_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Instance_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Instance_Id_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Instance_Id_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Instance_Id_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Instance_Id_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Instance_Id_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Instance_Id_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Instance_Id_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Instance_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Instance_Id_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Instance_Id_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Instance_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Instance_Id_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Instance_Id_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Instance_Id_Member (MT2_Smart_Ptr);
      MA1_Ptr.Instance_Id_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Instance_Id_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Instance_Id_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Instance_Id_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Instance_Id_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Instance_Id_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Instance_Id;

   procedure Test_Type_Id is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("TypeId");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Type_Id_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Type_Id_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Type_Id_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Type_Id_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Type_Id_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Type_Id_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Type_Id_Member.Is_Changed;
      MT1_Ptr.Type_Id_Member.Set_Val (Dots_Test.Parameter_Types.Type_Id_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Type_Id_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Type_Id_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Type_Id_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Type_Id_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Type_Id_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("TypeIdParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("TypeIdParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("TypeIdParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Type_Id_Member (MT2_Smart_Ptr, MT1_Ptr.Type_Id_Member.Get_Val);
      Print ("Val: ", Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Types_Property.Get_Type_Id_Member (MT2_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Type_Id_Member (MI_Smart_Ptr, MT2_Ptr.Type_Id_Member.Get_Val);
      Print ("Item Val: ", Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Types_Property.Get_Type_Id_Member (MI_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Type_Id_Member (MIA_Smart_Ptr, MT2_Ptr.Type_Id_Member.Get_Val);
      Print ("Item Array Val: ", Safir.Dob.Typesystem.Operations.Get_Name (MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Type_Id_Member.Get_Val));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Types_Property.Get_Type_Id_Member (EO_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Type_Id_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Type_Id_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Type_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Type_Id_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Type_Id_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Type_Id_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Type_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Type_Id_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Type_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Type_Id_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Type_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Type_Id_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Type_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Type_Id_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Arrays_Property.Get_Type_Id_Member (MA2_Smart_Ptr, Ix)));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Type_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Type_Id_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Arrays_Property.Get_Type_Id_Member (MI_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Type_Id_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Type_Id_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Arrays_Property.Get_Type_Id_Member (MIA_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Type_Id_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Type_Id_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Type_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Type_Id_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Operations.Get_Name (Dots_Test.Member_Arrays_Property.Get_Type_Id_Member (EO_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Type_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Type_Id_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Type_Id_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Type_Id_Member (MT2_Smart_Ptr);
      MA1_Ptr.Type_Id_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Type_Id_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Type_Id_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Type_Id_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Type_Id_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Type_Id_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Type_Id;

   procedure Test_Channel_Id is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("ChannelId");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Channel_Id_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Channel_Id_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Channel_Id_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Channel_Id_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Channel_Id_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Channel_Id_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Channel_Id_Member.Is_Changed;
      MT1_Ptr.Channel_Id_Member.Set_Val (Dots_Test.Parameter_Types.Channel_Id_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Channel_Id_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Channel_Id_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Channel_Id_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Channel_Id_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Channel_Id_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("ChannelIdParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("ChannelIdParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("ChannelIdParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Channel_Id_Member (MT2_Smart_Ptr, MT1_Ptr.Channel_Id_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Channel_Id_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Channel_Id_Member (MI_Smart_Ptr, MT2_Ptr.Channel_Id_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Channel_Id_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Channel_Id_Member (MIA_Smart_Ptr, MT2_Ptr.Channel_Id_Member.Get_Val);
      Print ("Item Array Val:", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Channel_Id_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Channel_Id_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Channel_Id_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Channel_Id_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Channel_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Channel_Id_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Channel_Id_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Channel_Id_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Channel_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Channel_Id_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Channel_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Channel_Id_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Channel_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Channel_Id_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Channel_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Channel_Id_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Channel_Id_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Channel_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Channel_Id_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Channel_Id_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Channel_Id_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Channel_Id_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Channel_Id_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Channel_Id_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Channel_Id_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Channel_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Channel_Id_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Channel_Id_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Channel_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Channel_Id_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Channel_Id_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Channel_Id_Member (MT2_Smart_Ptr);
      MA1_Ptr.Channel_Id_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Channel_Id_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Channel_Id_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Channel_Id_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Channel_Id_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Channel_Id_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Channel_Id;

   procedure Test_Handler_Id is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("HandlerId");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Handler_Id_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Handler_Id_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Handler_Id_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Handler_Id_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Handler_Id_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Handler_Id_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Handler_Id_Member.Is_Changed;
      MT1_Ptr.Handler_Id_Member.Set_Val (Dots_Test.Parameter_Types.Handler_Id_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Handler_Id_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Handler_Id_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Handler_Id_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Handler_Id_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Handler_Id_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("HandlerIdParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("HandlerIdParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("HandlerIdParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Handler_Id_Member (MT2_Smart_Ptr, MT1_Ptr.Handler_Id_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Handler_Id_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Handler_Id_Member (MI_Smart_Ptr, MT2_Ptr.Handler_Id_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Handler_Id_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Handler_Id_Member (MIA_Smart_Ptr, MT2_Ptr.Handler_Id_Member.Get_Val);
      Print ("Item Array Val:", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Handler_Id_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Handler_Id_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Handler_Id_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Handler_Id_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Handler_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Handler_Id_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Handler_Id_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Handler_Id_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Handler_Id_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Handler_Id_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Handler_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Handler_Id_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Handler_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Handler_Id_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Handler_Id_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Handler_Id_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Handler_Id_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Handler_Id_Member.Element (Ix).Set_Val (MA1_Ptr.Handler_Id_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Handler_Id_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Handler_Id_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Handler_Id_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Handler_Id_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Handler_Id_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Handler_Id_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Handler_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Handler_Id_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Handler_Id_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Handler_Id_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Handler_Id_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Handler_Id_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Handler_Id_Member (MT2_Smart_Ptr);
      MA1_Ptr.Handler_Id_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Handler_Id_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Handler_Id_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Handler_Id_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Handler_Id_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Handler_Id_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Handler_Id;

   procedure Test_Object is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Object");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Object_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Object_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Object_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Object_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Object_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Object_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Object_Member.Is_Changed;
      MT1_Ptr.Object_Member.Set_Ptr (Dots_Test.Parameter_Types.Object_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Object_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Object_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Object_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Object_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Object_Member_Member_Index));
      Print ("GetTypeId: ", Safir.Dob.Typesystem.Members.Get_Type_Id (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Object_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("ObjectParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("ObjectParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("ObjectParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Object_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Object_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Object_Member (MT2_Smart_Ptr, MT1_Ptr.Object_Member.Get_Ptr);
      Print ("Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Types_Property.Get_Object_Member (MT2_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Object_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Object_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Object_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Object_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Object_Member (MI_Smart_Ptr, MT2_Ptr.Object_Member.Get_Ptr);
      Print ("Item Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Types_Property.Get_Object_Member (MI_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Object_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Object_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Object_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Object_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Object_Member (MIA_Smart_Ptr, MT2_Ptr.Object_Member.Get_Ptr);
      Print ("Item Array Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Object_Member.Get_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Object_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Object_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Object_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Object_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Types_Property.Get_Object_Member (EO_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Object_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Object_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Object_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Object_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Object_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Object_Member.Element (Ix).Set_Ptr (Dots_Test.Parameter_Arrays.Object_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Object_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Object_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Object_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Object_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Object_Member.Element (Ix).Set_Ptr (MA1_Ptr.Object_Member.Element (Ix).Get_Ptr);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Object_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Object_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Object_Member (MA2_Smart_Ptr, Ix)));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Object_Member.Element (Ix).Set_Ptr (MA1_Ptr.Object_Member.Element (Ix).Get_Ptr);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Object_Member (MI_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Object_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Object_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Object_Member (MIA_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Object_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Object_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Object_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Object_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Object_Member (EO_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Object_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Object_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Object_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Object_Member (MT2_Smart_Ptr);
      MA1_Ptr.Object_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Object_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Object_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Object_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Object_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Object_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Object;

   procedure Test_Binary is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Binary");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Binary_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Binary_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Binary_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Binary_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Binary_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Binary_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Binary_Member.Is_Changed;
      MT1_Ptr.Binary_Member.Set_Val (Dots_Test.Parameter_Types.Binary_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Binary_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Binary_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Binary_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Binary_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Binary_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("BinaryParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("BinaryParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("BinaryParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Binary_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Binary_Member (MT2_Smart_Ptr, MT1_Ptr.Binary_Member.Get_Val.all);
      Print ("Val: ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (Dots_Test.Member_Types_Property.Get_Binary_Member (MT2_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Binary_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Binary_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Binary_Member (MI_Smart_Ptr, MT2_Ptr.Binary_Member.Get_Val.all);
      Print ("Item Val: ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (Dots_Test.Member_Types_Property.Get_Binary_Member (MI_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Binary_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Binary_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Binary_Member (MIA_Smart_Ptr, MT2_Ptr.Binary_Member.Get_Val.all);
      Print ("Item Array Val: ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Binary_Member.Get_Val.all));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Binary_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Binary_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (Dots_Test.Member_Types_Property.Get_Binary_Member (EO_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Binary_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Binary_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Binary_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Binary_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Binary_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Binary_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Binary_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Binary_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Binary_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Binary_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Binary_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Binary_Member.Element (Ix).Set_Val (MA1_Ptr.Binary_Member.Element (Ix).Get_Val.all);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Binary_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Binary_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (Dots_Test.Member_Arrays_Property.Get_Binary_Member (MA2_Smart_Ptr, Ix)));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Binary_Member.Element (Ix).Set_Val (MA1_Ptr.Binary_Member.Element (Ix).Get_Val.all);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (Dots_Test.Member_Arrays_Property.Get_Binary_Member (MI_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Binary_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Binary_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (Dots_Test.Member_Arrays_Property.Get_Binary_Member (MIA_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Binary_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Binary_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Binary_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Binary_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Utilities.Binary_To_Base_64 (Dots_Test.Member_Arrays_Property.Get_Binary_Member (EO_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Binary_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Binary_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Binary_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Binary_Member (MT2_Smart_Ptr);
      MA1_Ptr.Binary_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Binary_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Binary_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Binary_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Binary_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Binary_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Binary;

   procedure Test_Test_Class is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("TestClass");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Test_Class_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Test_Class_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Test_Class_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Test_Class_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Test_Class_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Test_Class_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Test_Class_Member.Is_Changed;
      MT1_Ptr.Test_Class_Member.Set_Ptr (Dots_Test.Parameter_Types.Test_Class_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Test_Class_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Test_Class_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Test_Class_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Test_Class_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Test_Class_Member_Member_Index));
      Print ("GetTypeId: ", Safir.Dob.Typesystem.Members.Get_Type_Id (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Test_Class_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("TestClassParameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("TestClassParameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("TestClassParameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Test_Class_Member (MT2_Smart_Ptr, MT1_Ptr.Test_Class_Member.Get_Ptr);
      Print ("Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Types_Property.Get_Test_Class_Member (MT2_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Test_Class_Member (MI_Smart_Ptr, MT2_Ptr.Test_Class_Member.Get_Ptr);
      Print ("Item Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Types_Property.Get_Test_Class_Member (MI_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Test_Class_Member (MIA_Smart_Ptr, MT2_Ptr.Test_Class_Member.Get_Ptr);
      Print ("Item Array Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Test_Class_Member.Get_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (MIA_Smart_Ptr);

      -- EmptyTest_Class
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Types_Property.Get_Test_Class_Member (EO_Smart_Ptr)));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Test_Class_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Test_Class_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Test_Class_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Test_Class_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Test_Class_Member.Element (Ix).Set_Ptr (Dots_Test.Parameter_Arrays.Test_Class_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Test_Class_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Test_Class_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Test_Class_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Test_Class_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Test_Class_Member.Element (Ix).Set_Ptr (MA1_Ptr.Test_Class_Member.Element (Ix).Get_Ptr);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Test_Class_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Test_Class_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Test_Class_Member (MA2_Smart_Ptr, Ix)));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Test_Class_Member.Element (Ix).Set_Ptr (MA1_Ptr.Test_Class_Member.Element (Ix).Get_Ptr);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Test_Class_Member (MI_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Test_Class_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Test_Class_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Test_Class_Member (MIA_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Test_Class_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Test_Class_Member (MIA_Smart_Ptr, Ix);

         --  EmptyTest_Class
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Test_Class_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Test_Class_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Safir.Dob.Typesystem.Serialization.To_Xml (Dots_Test.Member_Arrays_Property.Get_Test_Class_Member (EO_Smart_Ptr, Ix)));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Test_Class_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Test_Class_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Test_Class_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Test_Class_Member (MT2_Smart_Ptr);
      MA1_Ptr.Test_Class_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Test_Class_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Test_Class_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Test_Class_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Test_Class_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Test_Class_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Test_Class;

   procedure Test_Ampere_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Ampere32");
      Print ("MemberId: ", Dots_Test.Member_Types_Base.Ampere_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Ampere_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Ampere_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Ampere_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Ampere_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Ampere_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Ampere_32_Member.Is_Changed;
      MT1_Ptr.Ampere_32_Member.Set_Val (Dots_Test.Parameter_Types.Ampere_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Ampere_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Ampere_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Ampere_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Ampere_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types_Base.Ampere_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Ampere32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Ampere32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Ampere32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Ampere_32_Member (MT2_Smart_Ptr, MT1_Ptr.Ampere_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Ampere_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Ampere_32_Member (MI_Smart_Ptr, MT2_Ptr.Ampere_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Ampere_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Ampere_32_Member (MIA_Smart_Ptr, MT2_Ptr.Ampere_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Ampere_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Ampere_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Ampere_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Ampere_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Ampere_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Ampere_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Ampere_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Ampere_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Ampere_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Ampere_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Ampere_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Ampere_32_Member.Element (Ix).Set_Val (MA1_Ptr.Ampere_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Ampere_32_Member.Element (Ix).Set_Val (MA1_Ptr.Ampere_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Ampere_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Ampere_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Ampere_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Ampere_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Ampere_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Ampere_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Ampere_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Ampere_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Ampere_32;

   procedure Test_Cubic_Meter_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("CubicMeter32");
      Print ("MemberId: ", Dots_Test.Member_Types.Cubic_Meter_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Cubic_Meter_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Cubic_Meter_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Cubic_Meter_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Cubic_Meter_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Cubic_Meter_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Cubic_Meter_32_Member.Is_Changed;
      MT1_Ptr.Cubic_Meter_32_Member.Set_Val (Dots_Test.Parameter_Types.Cubic_Meter_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Cubic_Meter_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Cubic_Meter_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Cubic_Meter_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Cubic_Meter_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Cubic_Meter_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("CubicMeter32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("CubicMeter32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("CubicMeter32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Cubic_Meter_32_Member (MT2_Smart_Ptr, MT1_Ptr.Cubic_Meter_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Cubic_Meter_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Cubic_Meter_32_Member (MI_Smart_Ptr, MT2_Ptr.Cubic_Meter_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Cubic_Meter_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Cubic_Meter_32_Member (MIA_Smart_Ptr, MT2_Ptr.Cubic_Meter_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Cubic_Meter_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Cubic_Meter_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Cubic_Meter_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Cubic_Meter_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Cubic_Meter_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Cubic_Meter_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Cubic_Meter_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Cubic_Meter_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Cubic_Meter_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Cubic_Meter_32_Member.Element (Ix).Set_Val (MA1_Ptr.Cubic_Meter_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Cubic_Meter_32_Member.Element (Ix).Set_Val (MA1_Ptr.Cubic_Meter_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Cubic_Meter_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Cubic_Meter_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Cubic_Meter_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Cubic_Meter_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Cubic_Meter_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Cubic_Meter_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Cubic_Meter_32;

   procedure Test_Hertz_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Hertz32");
      Print ("MemberId: ", Dots_Test.Member_Types.Hertz_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Hertz_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Hertz_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Hertz_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Hertz_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Hertz_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Hertz_32_Member.Is_Changed;
      MT1_Ptr.Hertz_32_Member.Set_Val (Dots_Test.Parameter_Types.Hertz_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Hertz_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Hertz_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Hertz_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Hertz_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Hertz_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Hertz32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Hertz32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Hertz32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Hertz_32_Member (MT2_Smart_Ptr, MT1_Ptr.Hertz_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Hertz_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Hertz_32_Member (MI_Smart_Ptr, MT2_Ptr.Hertz_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Hertz_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Hertz_32_Member (MIA_Smart_Ptr, MT2_Ptr.Hertz_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Hertz_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Hertz_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Hertz_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Hertz_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Hertz_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Hertz_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Hertz_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Hertz_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Hertz_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Hertz_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Hertz_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Hertz_32_Member.Element (Ix).Set_Val (MA1_Ptr.Hertz_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Hertz_32_Member.Element (Ix).Set_Val (MA1_Ptr.Hertz_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Hertz_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Hertz_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Hertz_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Hertz_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Hertz_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Hertz_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Hertz_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Hertz_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Hertz_32;

   procedure Test_Joule_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Joule32");
      Print ("MemberId: ", Dots_Test.Member_Types.Joule_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Joule_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Joule_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Joule_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Joule_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Joule_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Joule_32_Member.Is_Changed;
      MT1_Ptr.Joule_32_Member.Set_Val (Dots_Test.Parameter_Types.Joule_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Joule_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Joule_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Joule_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Joule_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Joule_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Joule32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Joule32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Joule32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Joule_32_Member (MT2_Smart_Ptr, MT1_Ptr.Joule_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Joule_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Joule_32_Member (MI_Smart_Ptr, MT2_Ptr.Joule_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Joule_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Joule_32_Member (MIA_Smart_Ptr, MT2_Ptr.Joule_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Joule_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Joule_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Joule_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Joule_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Joule_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Joule_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Joule_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Joule_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Joule_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Joule_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Joule_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Joule_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Joule_32_Member.Element (Ix).Set_Val (MA1_Ptr.Joule_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Joule_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Joule_32_Member.Element (Ix).Set_Val (MA1_Ptr.Joule_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Joule_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Joule_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Joule_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Joule_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Joule_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Joule_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Joule_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Joule_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Joule_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Joule_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Joule_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Joule_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Joule_32;

   procedure Test_Kelvin_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Kelvin32");
      Print ("MemberId: ", Dots_Test.Member_Types.Kelvin_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Kelvin_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Kelvin_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Kelvin_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Kelvin_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Kelvin_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Kelvin_32_Member.Is_Changed;
      MT1_Ptr.Kelvin_32_Member.Set_Val (Dots_Test.Parameter_Types.Kelvin_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Kelvin_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Kelvin_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Kelvin_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kelvin_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kelvin_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kelvin32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kelvin32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kelvin32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kelvin_32_Member (MT2_Smart_Ptr, MT1_Ptr.Kelvin_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Kelvin_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kelvin_32_Member (MI_Smart_Ptr, MT2_Ptr.Kelvin_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Kelvin_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Kelvin_32_Member (MIA_Smart_Ptr, MT2_Ptr.Kelvin_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Kelvin_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Kelvin_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kelvin_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Kelvin_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Kelvin_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Kelvin_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Kelvin_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Kelvin_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Kelvin_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Kelvin_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Kelvin_32_Member.Element (Ix).Set_Val (MA1_Ptr.Kelvin_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Kelvin_32_Member.Element (Ix).Set_Val (MA1_Ptr.Kelvin_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Kelvin_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Kelvin_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Kelvin_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Kelvin_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Kelvin_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Kelvin_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Kelvin_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Kelvin_32;

   procedure Test_Kilogram_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Kilogram32");
      Print ("MemberId: ", Dots_Test.Member_Types.Kilogram_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Kilogram_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Kilogram_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Kilogram_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Kilogram_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Kilogram_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Kilogram_32_Member.Is_Changed;
      MT1_Ptr.Kilogram_32_Member.Set_Val (Dots_Test.Parameter_Types.Kilogram_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Kilogram_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Kilogram_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Kilogram_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kilogram_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kilogram_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kilogram32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kilogram32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kilogram32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kilogram_32_Member (MT2_Smart_Ptr, MT1_Ptr.Kilogram_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Kilogram_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kilogram_32_Member (MI_Smart_Ptr, MT2_Ptr.Kilogram_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Kilogram_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Kilogram_32_Member (MIA_Smart_Ptr, MT2_Ptr.Kilogram_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Kilogram_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Kilogram_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kilogram_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Kilogram_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Kilogram_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Kilogram_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Kilogram_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Kilogram_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Kilogram_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Kilogram_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Kilogram_32_Member.Element (Ix).Set_Val (MA1_Ptr.Kilogram_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Kilogram_32_Member.Element (Ix).Set_Val (MA1_Ptr.Kilogram_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Kilogram_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Kilogram_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Kilogram_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Kilogram_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Kilogram_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Kilogram_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Kilogram_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Kilogram_32;

   procedure Test_Meter_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Meter32");
      Print ("MemberId: ", Dots_Test.Member_Types.Meter_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Meter_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Meter_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Meter_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Meter_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Meter_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Meter_32_Member.Is_Changed;
      MT1_Ptr.Meter_32_Member.Set_Val (Dots_Test.Parameter_Types.Meter_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Meter_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Meter_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Meter_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Meter32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Meter32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Meter32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_32_Member (MT2_Smart_Ptr, MT1_Ptr.Meter_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Meter_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_32_Member (MI_Smart_Ptr, MT2_Ptr.Meter_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Meter_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Meter_32_Member (MIA_Smart_Ptr, MT2_Ptr.Meter_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Meter_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Meter_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Meter_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Meter_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Meter_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Meter_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Meter_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Meter_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Meter_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Meter_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Meter_32_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Meter_32_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Meter_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Meter_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Meter_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Meter_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Meter_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Meter_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Meter_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Meter_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Meter_32;

   procedure Test_Meter_Per_Second_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("MeterPerSecond32");
      Print ("MemberId: ", Dots_Test.Member_Types.Meter_Per_Second_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Meter_Per_Second_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Meter_Per_Second_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Meter_Per_Second_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Meter_Per_Second_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Meter_Per_Second_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Meter_Per_Second_32_Member.Is_Changed;
      MT1_Ptr.Meter_Per_Second_32_Member.Set_Val (Dots_Test.Parameter_Types.Meter_Per_Second_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Meter_Per_Second_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Meter_Per_Second_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Meter_Per_Second_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecond32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecond32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecond32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_32_Member (MT2_Smart_Ptr, MT1_Ptr.Meter_Per_Second_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_32_Member (MI_Smart_Ptr, MT2_Ptr.Meter_Per_Second_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_32_Member (MIA_Smart_Ptr, MT2_Ptr.Meter_Per_Second_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Meter_Per_Second_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Meter_Per_Second_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Meter_Per_Second_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Meter_Per_Second_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Meter_Per_Second_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Meter_Per_Second_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Meter_Per_Second_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Meter_Per_Second_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Meter_Per_Second_32_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Meter_Per_Second_32_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Meter_Per_Second_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Meter_Per_Second_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Meter_Per_Second_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Meter_Per_Second_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Meter_Per_Second_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Meter_Per_Second_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Meter_Per_Second_32;

   procedure Test_Meter_Per_Second_Squared_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("MeterPerSecondSquared32");
      Print ("MemberId: ", Dots_Test.Member_Types.Meter_Per_Second_Squared_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Meter_Per_Second_Squared_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Meter_Per_Second_Squared_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Meter_Per_Second_Squared_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Meter_Per_Second_Squared_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Meter_Per_Second_Squared_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Meter_Per_Second_Squared_32_Member.Is_Changed;
      MT1_Ptr.Meter_Per_Second_Squared_32_Member.Set_Val (Dots_Test.Parameter_Types.Meter_Per_Second_Squared_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Meter_Per_Second_Squared_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Meter_Per_Second_Squared_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Meter_Per_Second_Squared_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_Squared_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_Squared_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecondSquared32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecondSquared32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecondSquared32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr, MT1_Ptr.Meter_Per_Second_Squared_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr, MT2_Ptr.Meter_Per_Second_Squared_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr, MT2_Ptr.Meter_Per_Second_Squared_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Meter_Per_Second_Squared_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Meter_Per_Second_Squared_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Meter_Per_Second_Squared_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Meter_Per_Second_Squared_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Meter_Per_Second_Squared_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Meter_Per_Second_Squared_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Meter_Per_Second_Squared_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Meter_Per_Second_Squared_32;

   procedure Test_Newton_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Newton32");
      Print ("MemberId: ", Dots_Test.Member_Types.Newton_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Newton_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Newton_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Newton_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Newton_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Newton_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Newton_32_Member.Is_Changed;
      MT1_Ptr.Newton_32_Member.Set_Val (Dots_Test.Parameter_Types.Newton_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Newton_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Newton_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Newton_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Newton_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Newton_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Newton32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Newton32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Newton32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Newton_32_Member (MT2_Smart_Ptr, MT1_Ptr.Newton_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Newton_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Newton_32_Member (MI_Smart_Ptr, MT2_Ptr.Newton_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Newton_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Newton_32_Member (MIA_Smart_Ptr, MT2_Ptr.Newton_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Newton_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Newton_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Newton_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Newton_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Newton_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Newton_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Newton_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Newton_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Newton_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Newton_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Newton_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Newton_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Newton_32_Member.Element (Ix).Set_Val (MA1_Ptr.Newton_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Newton_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Newton_32_Member.Element (Ix).Set_Val (MA1_Ptr.Newton_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Newton_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Newton_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Newton_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Newton_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Newton_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Newton_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Newton_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Newton_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Newton_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Newton_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Newton_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Newton_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Newton_32;

   procedure Test_Pascal_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Pascal32");
      Print ("MemberId: ", Dots_Test.Member_Types.Pascal_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Pascal_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Pascal_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Pascal_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Pascal_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Pascal_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Pascal_32_Member.Is_Changed;
      MT1_Ptr.Pascal_32_Member.Set_Val (Dots_Test.Parameter_Types.Pascal_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Pascal_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Pascal_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Pascal_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Pascal_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Pascal_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Pascal32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Pascal32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Pascal32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Pascal_32_Member (MT2_Smart_Ptr, MT1_Ptr.Pascal_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Pascal_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Pascal_32_Member (MI_Smart_Ptr, MT2_Ptr.Pascal_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Pascal_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Pascal_32_Member (MIA_Smart_Ptr, MT2_Ptr.Pascal_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Pascal_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Pascal_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Pascal_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Pascal_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Pascal_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Pascal_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Pascal_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Pascal_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Pascal_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Pascal_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Pascal_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Pascal_32_Member.Element (Ix).Set_Val (MA1_Ptr.Pascal_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Pascal_32_Member.Element (Ix).Set_Val (MA1_Ptr.Pascal_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Pascal_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Pascal_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Pascal_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Pascal_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Pascal_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Pascal_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Pascal_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Pascal_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Pascal_32;

   procedure Test_Radian_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Radian32");
      Print ("MemberId: ", Dots_Test.Member_Types.Radian_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Radian_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Radian_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Radian_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Radian_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Radian_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Radian_32_Member.Is_Changed;
      MT1_Ptr.Radian_32_Member.Set_Val (Dots_Test.Parameter_Types.Radian_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Radian_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Radian_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Radian_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Radian32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Radian32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Radian32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_32_Member (MT2_Smart_Ptr, MT1_Ptr.Radian_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Radian_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_32_Member (MI_Smart_Ptr, MT2_Ptr.Radian_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Radian_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Radian_32_Member (MIA_Smart_Ptr, MT2_Ptr.Radian_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Radian_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Radian_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Radian_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Radian_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Radian_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Radian_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Radian_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Radian_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Radian_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Radian_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Radian_32_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Radian_32_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Radian_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Radian_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Radian_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Radian_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Radian_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Radian_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Radian_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Radian_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Radian_32;

   procedure Test_Radian_Per_Second_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("RadianPerSecond32");
      Print ("MemberId: ", Dots_Test.Member_Types.Radian_Per_Second_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Radian_Per_Second_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Radian_Per_Second_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Radian_Per_Second_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Radian_Per_Second_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Radian_Per_Second_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Radian_Per_Second_32_Member.Is_Changed;
      MT1_Ptr.Radian_Per_Second_32_Member.Set_Val (Dots_Test.Parameter_Types.Radian_Per_Second_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Radian_Per_Second_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Radian_Per_Second_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Radian_Per_Second_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecond32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecond32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecond32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_32_Member (MT2_Smart_Ptr, MT1_Ptr.Radian_Per_Second_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_32_Member (MI_Smart_Ptr, MT2_Ptr.Radian_Per_Second_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_32_Member (MIA_Smart_Ptr, MT2_Ptr.Radian_Per_Second_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Radian_Per_Second_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Radian_Per_Second_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Radian_Per_Second_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Radian_Per_Second_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Radian_Per_Second_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Radian_Per_Second_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Radian_Per_Second_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Radian_Per_Second_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Radian_Per_Second_32_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Radian_Per_Second_32_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Radian_Per_Second_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Radian_Per_Second_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Radian_Per_Second_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Radian_Per_Second_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Radian_Per_Second_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Radian_Per_Second_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Radian_Per_Second_32;

   procedure Test_Radian_Per_Second_Squared_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("RadianPerSecondSquared32");
      Print ("MemberId: ", Dots_Test.Member_Types.Radian_Per_Second_Squared_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Radian_Per_Second_Squared_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Radian_Per_Second_Squared_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Radian_Per_Second_Squared_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Radian_Per_Second_Squared_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Radian_Per_Second_Squared_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Radian_Per_Second_Squared_32_Member.Is_Changed;
      MT1_Ptr.Radian_Per_Second_Squared_32_Member.Set_Val (Dots_Test.Parameter_Types.Radian_Per_Second_Squared_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Radian_Per_Second_Squared_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Radian_Per_Second_Squared_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Radian_Per_Second_Squared_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_Squared_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_Squared_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecondSquared32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecondSquared32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecondSquared32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr, MT1_Ptr.Radian_Per_Second_Squared_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr, MT2_Ptr.Radian_Per_Second_Squared_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr, MT2_Ptr.Radian_Per_Second_Squared_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Radian_Per_Second_Squared_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Radian_Per_Second_Squared_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Radian_Per_Second_Squared_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Radian_Per_Second_Squared_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Radian_Per_Second_Squared_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Radian_Per_Second_Squared_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Radian_Per_Second_Squared_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Radian_Per_Second_Squared_32;

   procedure Test_Second_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Second32");
      Print ("MemberId: ", Dots_Test.Member_Types.Second_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Second_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Second_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Second_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Second_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Second_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Second_32_Member.Is_Changed;
      MT1_Ptr.Second_32_Member.Set_Val (Dots_Test.Parameter_Types.Second_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Second_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Second_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Second_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Second_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Second_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Second32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Second32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Second32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Second_32_Member (MT2_Smart_Ptr, MT1_Ptr.Second_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Second_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Second_32_Member (MI_Smart_Ptr, MT2_Ptr.Second_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Second_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Second_32_Member (MIA_Smart_Ptr, MT2_Ptr.Second_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Second_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Second_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Second_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Second_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Second_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Second_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Second_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Second_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Second_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Second_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Second_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Second_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Second_32_Member.Element (Ix).Set_Val (MA1_Ptr.Second_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Second_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Second_32_Member.Element (Ix).Set_Val (MA1_Ptr.Second_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Second_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Second_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Second_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Second_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Second_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Second_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Second_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Second_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Second_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Second_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Second_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Second_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Second_32;

   procedure Test_Square_Meter_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("SquareMeter32");
      Print ("MemberId: ", Dots_Test.Member_Types.Square_Meter_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Square_Meter_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Square_Meter_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Square_Meter_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Square_Meter_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Square_Meter_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Square_Meter_32_Member.Is_Changed;
      MT1_Ptr.Square_Meter_32_Member.Set_Val (Dots_Test.Parameter_Types.Square_Meter_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Square_Meter_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Square_Meter_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Square_Meter_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Square_Meter_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Square_Meter_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("SquareMeter32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("SquareMeter32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("SquareMeter32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Square_Meter_32_Member (MT2_Smart_Ptr, MT1_Ptr.Square_Meter_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Square_Meter_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Square_Meter_32_Member (MI_Smart_Ptr, MT2_Ptr.Square_Meter_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Square_Meter_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Square_Meter_32_Member (MIA_Smart_Ptr, MT2_Ptr.Square_Meter_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Square_Meter_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Square_Meter_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Square_Meter_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Square_Meter_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Square_Meter_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Square_Meter_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Square_Meter_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Square_Meter_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Square_Meter_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Square_Meter_32_Member.Element (Ix).Set_Val (MA1_Ptr.Square_Meter_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Square_Meter_32_Member.Element (Ix).Set_Val (MA1_Ptr.Square_Meter_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Square_Meter_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Square_Meter_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Square_Meter_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Square_Meter_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Square_Meter_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Square_Meter_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Square_Meter_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Square_Meter_32;

   procedure Test_Steradian_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Steradian32");
      Print ("MemberId: ", Dots_Test.Member_Types.Steradian_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Steradian_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Steradian_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Steradian_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Steradian_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Steradian_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Steradian_32_Member.Is_Changed;
      MT1_Ptr.Steradian_32_Member.Set_Val (Dots_Test.Parameter_Types.Steradian_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Steradian_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Steradian_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Steradian_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Steradian_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Steradian_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Steradian32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Steradian32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Steradian32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Steradian_32_Member (MT2_Smart_Ptr, MT1_Ptr.Steradian_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Steradian_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Steradian_32_Member (MI_Smart_Ptr, MT2_Ptr.Steradian_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Steradian_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Steradian_32_Member (MIA_Smart_Ptr, MT2_Ptr.Steradian_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Steradian_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Steradian_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Steradian_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Steradian_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Steradian_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Steradian_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Steradian_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Steradian_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Steradian_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Steradian_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Steradian_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Steradian_32_Member.Element (Ix).Set_Val (MA1_Ptr.Steradian_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Steradian_32_Member.Element (Ix).Set_Val (MA1_Ptr.Steradian_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Steradian_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Steradian_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Steradian_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Steradian_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Steradian_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Steradian_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Steradian_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Steradian_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Steradian_32;

   procedure Test_Volt_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Volt32");
      Print ("MemberId: ", Dots_Test.Member_Types.Volt_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Volt_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Volt_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Volt_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Volt_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Volt_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Volt_32_Member.Is_Changed;
      MT1_Ptr.Volt_32_Member.Set_Val (Dots_Test.Parameter_Types.Volt_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Volt_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Volt_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Volt_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Volt_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Volt_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Volt32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Volt32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Volt32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Volt_32_Member (MT2_Smart_Ptr, MT1_Ptr.Volt_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Volt_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Volt_32_Member (MI_Smart_Ptr, MT2_Ptr.Volt_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Volt_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Volt_32_Member (MIA_Smart_Ptr, MT2_Ptr.Volt_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Volt_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Volt_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Volt_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Volt_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Volt_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Volt_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Volt_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Volt_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Volt_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Volt_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Volt_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Volt_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Volt_32_Member.Element (Ix).Set_Val (MA1_Ptr.Volt_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Volt_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Volt_32_Member.Element (Ix).Set_Val (MA1_Ptr.Volt_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Volt_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Volt_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Volt_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Volt_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Volt_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Volt_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Volt_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Volt_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Volt_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Volt_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Volt_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Volt_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Volt_32;

   procedure Test_Watt_32 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Watt32");
      Print ("MemberId: ", Dots_Test.Member_Types.Watt_32_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Watt_32_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Watt_32_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Watt_32_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Watt_32_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Watt_32_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Watt_32_Member.Is_Changed;
      MT1_Ptr.Watt_32_Member.Set_Val (Dots_Test.Parameter_Types.Watt_32_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Watt_32_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Watt_32_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Watt_32_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Watt_32_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Watt_32_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Watt32Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Watt32Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Watt32Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Watt_32_Member (MT2_Smart_Ptr, MT1_Ptr.Watt_32_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Watt_32_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Watt_32_Member (MI_Smart_Ptr, MT2_Ptr.Watt_32_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Watt_32_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Watt_32_Member (MIA_Smart_Ptr, MT2_Ptr.Watt_32_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Watt_32_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Watt_32_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Watt_32_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Watt_32_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Watt_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Watt_32_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Watt_32_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Watt_32_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Watt_32_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Watt_32_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Watt_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Watt_32_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Watt_32_Member.Element (Ix).Set_Val (MA1_Ptr.Watt_32_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_32_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Watt_32_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_32_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Watt_32_Member.Element (Ix).Set_Val (MA1_Ptr.Watt_32_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_32_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_32_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Watt_32_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_32_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_32_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Watt_32_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Watt_32_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_32_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_32_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Watt_32_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Watt_32_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Watt_32_Member (MT2_Smart_Ptr);
      MA1_Ptr.Watt_32_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Watt_32_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Watt_32_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Watt_32_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Watt_32_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Watt_32_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Watt_32;

   procedure Test_Ampere_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Ampere64");
      Print ("MemberId: ", Dots_Test.Member_Types.Ampere_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Ampere_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Ampere_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Ampere_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Ampere_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Ampere_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Ampere_64_Member.Is_Changed;
      MT1_Ptr.Ampere_64_Member.Set_Val (Dots_Test.Parameter_Types.Ampere_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Ampere_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Ampere_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Ampere_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Ampere_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Ampere_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Ampere64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Ampere64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Ampere64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Ampere_64_Member (MT2_Smart_Ptr, MT1_Ptr.Ampere_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Ampere_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Ampere_64_Member (MI_Smart_Ptr, MT2_Ptr.Ampere_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Ampere_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Ampere_64_Member (MIA_Smart_Ptr, MT2_Ptr.Ampere_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Ampere_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Ampere_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Ampere_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Ampere_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Ampere_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Ampere_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Ampere_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Ampere_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Ampere_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Ampere_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Ampere_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Ampere_64_Member.Element (Ix).Set_Val (MA1_Ptr.Ampere_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Ampere_64_Member.Element (Ix).Set_Val (MA1_Ptr.Ampere_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Ampere_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Ampere_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Ampere_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Ampere_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Ampere_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Ampere_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Ampere_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Ampere_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Ampere_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Ampere_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Ampere_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Ampere_64;

   procedure Test_Cubic_Meter_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("CubicMeter64");
      Print ("MemberId: ", Dots_Test.Member_Types.Cubic_Meter_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Cubic_Meter_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Cubic_Meter_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Cubic_Meter_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Cubic_Meter_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Cubic_Meter_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Cubic_Meter_64_Member.Is_Changed;
      MT1_Ptr.Cubic_Meter_64_Member.Set_Val (Dots_Test.Parameter_Types.Cubic_Meter_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Cubic_Meter_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Cubic_Meter_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Cubic_Meter_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Cubic_Meter_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Cubic_Meter_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("CubicMeter64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("CubicMeter64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("CubicMeter64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Cubic_Meter_64_Member (MT2_Smart_Ptr, MT1_Ptr.Cubic_Meter_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Cubic_Meter_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Cubic_Meter_64_Member (MI_Smart_Ptr, MT2_Ptr.Cubic_Meter_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Cubic_Meter_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Cubic_Meter_64_Member (MIA_Smart_Ptr, MT2_Ptr.Cubic_Meter_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Cubic_Meter_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Cubic_Meter_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Cubic_Meter_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Cubic_Meter_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Cubic_Meter_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Cubic_Meter_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Cubic_Meter_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Cubic_Meter_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Cubic_Meter_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Cubic_Meter_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Cubic_Meter_64_Member.Element (Ix).Set_Val (MA1_Ptr.Cubic_Meter_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Cubic_Meter_64_Member.Element (Ix).Set_Val (MA1_Ptr.Cubic_Meter_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Cubic_Meter_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Cubic_Meter_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Cubic_Meter_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Cubic_Meter_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Cubic_Meter_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Cubic_Meter_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Cubic_Meter_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Cubic_Meter_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Cubic_Meter_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Cubic_Meter_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Cubic_Meter_64;

   procedure Test_Hertz_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Hertz64");
      Print ("MemberId: ", Dots_Test.Member_Types.Hertz_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Hertz_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Hertz_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Hertz_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Hertz_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Hertz_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Hertz_64_Member.Is_Changed;
      MT1_Ptr.Hertz_64_Member.Set_Val (Dots_Test.Parameter_Types.Hertz_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Hertz_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Hertz_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Hertz_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Hertz_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Hertz_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Hertz64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Hertz64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Hertz64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Hertz_64_Member (MT2_Smart_Ptr, MT1_Ptr.Hertz_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Hertz_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Hertz_64_Member (MI_Smart_Ptr, MT2_Ptr.Hertz_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Hertz_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Hertz_64_Member (MIA_Smart_Ptr, MT2_Ptr.Hertz_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Hertz_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Hertz_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Hertz_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Hertz_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Hertz_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Hertz_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Hertz_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Hertz_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Hertz_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Hertz_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Hertz_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Hertz_64_Member.Element (Ix).Set_Val (MA1_Ptr.Hertz_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Hertz_64_Member.Element (Ix).Set_Val (MA1_Ptr.Hertz_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Hertz_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Hertz_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Hertz_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Hertz_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Hertz_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Hertz_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Hertz_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Hertz_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Hertz_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Hertz_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Hertz_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Hertz_64;

   procedure Test_Joule_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Joule64");
      Print ("MemberId: ", Dots_Test.Member_Types.Joule_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Joule_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Joule_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Joule_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Joule_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Joule_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Joule_64_Member.Is_Changed;
      MT1_Ptr.Joule_64_Member.Set_Val (Dots_Test.Parameter_Types.Joule_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Joule_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Joule_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Joule_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Joule_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Joule_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Joule64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Joule64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Joule64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Joule_64_Member (MT2_Smart_Ptr, MT1_Ptr.Joule_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Joule_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Joule_64_Member (MI_Smart_Ptr, MT2_Ptr.Joule_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Joule_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Joule_64_Member (MIA_Smart_Ptr, MT2_Ptr.Joule_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Joule_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Joule_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Joule_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Joule_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Joule_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Joule_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Joule_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Joule_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Joule_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Joule_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Joule_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Joule_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Joule_64_Member.Element (Ix).Set_Val (MA1_Ptr.Joule_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Joule_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Joule_64_Member.Element (Ix).Set_Val (MA1_Ptr.Joule_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Joule_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Joule_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Joule_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Joule_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Joule_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Joule_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Joule_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Joule_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Joule_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Joule_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Joule_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Joule_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Joule_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Joule_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Joule_64;

   procedure Test_Kelvin_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Kelvin64");
      Print ("MemberId: ", Dots_Test.Member_Types.Kelvin_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Kelvin_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Kelvin_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Kelvin_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Kelvin_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Kelvin_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Kelvin_64_Member.Is_Changed;
      MT1_Ptr.Kelvin_64_Member.Set_Val (Dots_Test.Parameter_Types.Kelvin_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Kelvin_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Kelvin_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Kelvin_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kelvin_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kelvin_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kelvin64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kelvin64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kelvin64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kelvin_64_Member (MT2_Smart_Ptr, MT1_Ptr.Kelvin_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Kelvin_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kelvin_64_Member (MI_Smart_Ptr, MT2_Ptr.Kelvin_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Kelvin_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Kelvin_64_Member (MIA_Smart_Ptr, MT2_Ptr.Kelvin_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Kelvin_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Kelvin_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kelvin_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Kelvin_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Kelvin_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Kelvin_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Kelvin_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Kelvin_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Kelvin_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Kelvin_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Kelvin_64_Member.Element (Ix).Set_Val (MA1_Ptr.Kelvin_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Kelvin_64_Member.Element (Ix).Set_Val (MA1_Ptr.Kelvin_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kelvin_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kelvin_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Kelvin_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Kelvin_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Kelvin_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Kelvin_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Kelvin_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Kelvin_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Kelvin_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Kelvin_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Kelvin_64;

   procedure Test_Kilogram_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Kilogram64");
      Print ("MemberId: ", Dots_Test.Member_Types.Kilogram_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Kilogram_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Kilogram_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Kilogram_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Kilogram_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Kilogram_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Kilogram_64_Member.Is_Changed;
      MT1_Ptr.Kilogram_64_Member.Set_Val (Dots_Test.Parameter_Types.Kilogram_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Kilogram_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Kilogram_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Kilogram_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kilogram_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Kilogram_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kilogram64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kilogram64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Kilogram64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kilogram_64_Member (MT2_Smart_Ptr, MT1_Ptr.Kilogram_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Kilogram_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Kilogram_64_Member (MI_Smart_Ptr, MT2_Ptr.Kilogram_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Kilogram_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Kilogram_64_Member (MIA_Smart_Ptr, MT2_Ptr.Kilogram_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Kilogram_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Kilogram_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Kilogram_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Kilogram_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Kilogram_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Kilogram_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Kilogram_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Kilogram_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Kilogram_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Kilogram_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Kilogram_64_Member.Element (Ix).Set_Val (MA1_Ptr.Kilogram_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Kilogram_64_Member.Element (Ix).Set_Val (MA1_Ptr.Kilogram_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Kilogram_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Kilogram_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Kilogram_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Kilogram_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Kilogram_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Kilogram_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Kilogram_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Kilogram_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Kilogram_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Kilogram_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Kilogram_64;

   procedure Test_Meter_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Meter64");
      Print ("MemberId: ", Dots_Test.Member_Types.Meter_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Meter_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Meter_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Meter_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Meter_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Meter_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Meter_64_Member.Is_Changed;
      MT1_Ptr.Meter_64_Member.Set_Val (Dots_Test.Parameter_Types.Meter_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Meter_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Meter_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Meter_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Meter64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Meter64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Meter64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_64_Member (MT2_Smart_Ptr, MT1_Ptr.Meter_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Meter_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_64_Member (MI_Smart_Ptr, MT2_Ptr.Meter_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Meter_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Meter_64_Member (MIA_Smart_Ptr, MT2_Ptr.Meter_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Meter_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Meter_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Meter_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Meter_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Meter_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Meter_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Meter_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Meter_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Meter_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Meter_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Meter_64_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Meter_64_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Meter_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Meter_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Meter_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Meter_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Meter_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Meter_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Meter_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Meter_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Meter_64;

   procedure Test_Meter_Per_Second_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("MeterPerSecond64");
      Print ("MemberId: ", Dots_Test.Member_Types.Meter_Per_Second_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Meter_Per_Second_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Meter_Per_Second_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Meter_Per_Second_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Meter_Per_Second_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Meter_Per_Second_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Meter_Per_Second_64_Member.Is_Changed;
      MT1_Ptr.Meter_Per_Second_64_Member.Set_Val (Dots_Test.Parameter_Types.Meter_Per_Second_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Meter_Per_Second_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Meter_Per_Second_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Meter_Per_Second_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecond64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecond64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecond64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_64_Member (MT2_Smart_Ptr, MT1_Ptr.Meter_Per_Second_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_64_Member (MI_Smart_Ptr, MT2_Ptr.Meter_Per_Second_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_64_Member (MIA_Smart_Ptr, MT2_Ptr.Meter_Per_Second_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Meter_Per_Second_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Meter_Per_Second_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Meter_Per_Second_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Meter_Per_Second_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Meter_Per_Second_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Meter_Per_Second_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Meter_Per_Second_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Meter_Per_Second_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Meter_Per_Second_64_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Meter_Per_Second_64_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Meter_Per_Second_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Meter_Per_Second_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Meter_Per_Second_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Meter_Per_Second_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Meter_Per_Second_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Meter_Per_Second_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Meter_Per_Second_64;

   procedure Test_Meter_Per_Second_Squared_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("MeterPerSecondSquared64");
      Print ("MemberId: ", Dots_Test.Member_Types.Meter_Per_Second_Squared_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Meter_Per_Second_Squared_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Meter_Per_Second_Squared_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Meter_Per_Second_Squared_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Meter_Per_Second_Squared_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Meter_Per_Second_Squared_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Meter_Per_Second_Squared_64_Member.Is_Changed;
      MT1_Ptr.Meter_Per_Second_Squared_64_Member.Set_Val (Dots_Test.Parameter_Types.Meter_Per_Second_Squared_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Meter_Per_Second_Squared_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Meter_Per_Second_Squared_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Meter_Per_Second_Squared_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_Squared_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Meter_Per_Second_Squared_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecondSquared64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecondSquared64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("MeterPerSecondSquared64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr, MT1_Ptr.Meter_Per_Second_Squared_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr, MT2_Ptr.Meter_Per_Second_Squared_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr, MT2_Ptr.Meter_Per_Second_Squared_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Meter_Per_Second_Squared_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Meter_Per_Second_Squared_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Meter_Per_Second_Squared_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Set_Val (MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Meter_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Meter_Per_Second_Squared_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Meter_Per_Second_Squared_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Meter_Per_Second_Squared_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Meter_Per_Second_Squared_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Meter_Per_Second_Squared_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Meter_Per_Second_Squared_64;

   procedure Test_Newton_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Newton64");
      Print ("MemberId: ", Dots_Test.Member_Types.Newton_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Newton_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Newton_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Newton_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Newton_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Newton_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Newton_64_Member.Is_Changed;
      MT1_Ptr.Newton_64_Member.Set_Val (Dots_Test.Parameter_Types.Newton_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Newton_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Newton_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Newton_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Newton_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Newton_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Newton64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Newton64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Newton64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Newton_64_Member (MT2_Smart_Ptr, MT1_Ptr.Newton_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Newton_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Newton_64_Member (MI_Smart_Ptr, MT2_Ptr.Newton_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Newton_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Newton_64_Member (MIA_Smart_Ptr, MT2_Ptr.Newton_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Newton_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Newton_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Newton_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Newton_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Newton_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Newton_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Newton_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Newton_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Newton_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Newton_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Newton_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Newton_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Newton_64_Member.Element (Ix).Set_Val (MA1_Ptr.Newton_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Newton_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Newton_64_Member.Element (Ix).Set_Val (MA1_Ptr.Newton_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Newton_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Newton_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Newton_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Newton_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Newton_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Newton_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Newton_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Newton_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Newton_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Newton_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Newton_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Newton_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Newton_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Newton_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Newton_64;

   procedure Test_Pascal_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Pascal64");
      Print ("MemberId: ", Dots_Test.Member_Types.Pascal_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Pascal_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Pascal_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Pascal_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Pascal_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Pascal_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Pascal_64_Member.Is_Changed;
      MT1_Ptr.Pascal_64_Member.Set_Val (Dots_Test.Parameter_Types.Pascal_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Pascal_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Pascal_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Pascal_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Pascal_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Pascal_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Pascal64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Pascal64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Pascal64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Pascal_64_Member (MT2_Smart_Ptr, MT1_Ptr.Pascal_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Pascal_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Pascal_64_Member (MI_Smart_Ptr, MT2_Ptr.Pascal_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Pascal_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Pascal_64_Member (MIA_Smart_Ptr, MT2_Ptr.Pascal_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Pascal_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Pascal_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Pascal_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Pascal_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Pascal_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Pascal_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Pascal_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Pascal_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Pascal_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Pascal_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Pascal_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Pascal_64_Member.Element (Ix).Set_Val (MA1_Ptr.Pascal_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Pascal_64_Member.Element (Ix).Set_Val (MA1_Ptr.Pascal_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Pascal_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Pascal_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Pascal_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Pascal_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Pascal_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Pascal_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Pascal_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Pascal_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Pascal_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Pascal_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Pascal_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Pascal_64;

   procedure Test_Radian_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Radian64");
      Print ("MemberId: ", Dots_Test.Member_Types.Radian_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Radian_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Radian_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Radian_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Radian_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Radian_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Radian_64_Member.Is_Changed;
      MT1_Ptr.Radian_64_Member.Set_Val (Dots_Test.Parameter_Types.Radian_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Radian_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Radian_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Radian_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Radian64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Radian64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Radian64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_64_Member (MT2_Smart_Ptr, MT1_Ptr.Radian_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Radian_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_64_Member (MI_Smart_Ptr, MT2_Ptr.Radian_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Radian_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Radian_64_Member (MIA_Smart_Ptr, MT2_Ptr.Radian_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Radian_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Radian_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Radian_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Radian_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Radian_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Radian_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Radian_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Radian_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Radian_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Radian_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Radian_64_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Radian_64_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Radian_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Radian_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Radian_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Radian_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Radian_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Radian_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Radian_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Radian_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Radian_64;

   procedure Test_Radian_Per_Second_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("RadianPerSecond64");
      Print ("MemberId: ", Dots_Test.Member_Types.Radian_Per_Second_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Radian_Per_Second_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Radian_Per_Second_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Radian_Per_Second_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Radian_Per_Second_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Radian_Per_Second_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Radian_Per_Second_64_Member.Is_Changed;
      MT1_Ptr.Radian_Per_Second_64_Member.Set_Val (Dots_Test.Parameter_Types.Radian_Per_Second_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Radian_Per_Second_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Radian_Per_Second_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Radian_Per_Second_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecond64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecond64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecond64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_64_Member (MT2_Smart_Ptr, MT1_Ptr.Radian_Per_Second_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_64_Member (MI_Smart_Ptr, MT2_Ptr.Radian_Per_Second_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_64_Member (MIA_Smart_Ptr, MT2_Ptr.Radian_Per_Second_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Radian_Per_Second_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Radian_Per_Second_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Radian_Per_Second_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Radian_Per_Second_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Radian_Per_Second_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Radian_Per_Second_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Radian_Per_Second_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Radian_Per_Second_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Radian_Per_Second_64_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Radian_Per_Second_64_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Radian_Per_Second_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Radian_Per_Second_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Radian_Per_Second_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Radian_Per_Second_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Radian_Per_Second_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Radian_Per_Second_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Radian_Per_Second_64;

   procedure Test_Radian_Per_Second_Squared_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("RadianPerSecondSquared64");
      Print ("MemberId: ", Dots_Test.Member_Types.Radian_Per_Second_Squared_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Radian_Per_Second_Squared_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Radian_Per_Second_Squared_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Radian_Per_Second_Squared_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Radian_Per_Second_Squared_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Radian_Per_Second_Squared_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Radian_Per_Second_Squared_64_Member.Is_Changed;
      MT1_Ptr.Radian_Per_Second_Squared_64_Member.Set_Val (Dots_Test.Parameter_Types.Radian_Per_Second_Squared_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Radian_Per_Second_Squared_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Radian_Per_Second_Squared_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Radian_Per_Second_Squared_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_Squared_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Radian_Per_Second_Squared_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecondSquared64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecondSquared64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("RadianPerSecondSquared64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr, MT1_Ptr.Radian_Per_Second_Squared_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr, MT2_Ptr.Radian_Per_Second_Squared_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr, MT2_Ptr.Radian_Per_Second_Squared_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Radian_Per_Second_Squared_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Radian_Per_Second_Squared_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Radian_Per_Second_Squared_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Set_Val (MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Radian_Per_Second_Squared_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Radian_Per_Second_Squared_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Radian_Per_Second_Squared_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Radian_Per_Second_Squared_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Radian_Per_Second_Squared_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Radian_Per_Second_Squared_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Radian_Per_Second_Squared_64;

   procedure Test_Second_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Second64");
      Print ("MemberId: ", Dots_Test.Member_Types.Second_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Second_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Second_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Second_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Second_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Second_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Second_64_Member.Is_Changed;
      MT1_Ptr.Second_64_Member.Set_Val (Dots_Test.Parameter_Types.Second_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Second_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Second_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Second_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Second_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Second_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Second64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Second64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Second64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Second_64_Member (MT2_Smart_Ptr, MT1_Ptr.Second_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Second_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Second_64_Member (MI_Smart_Ptr, MT2_Ptr.Second_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Second_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Second_64_Member (MIA_Smart_Ptr, MT2_Ptr.Second_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Second_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Second_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Second_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Second_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Second_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Second_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Second_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Second_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Second_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Second_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Second_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Second_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Second_64_Member.Element (Ix).Set_Val (MA1_Ptr.Second_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Second_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Second_64_Member.Element (Ix).Set_Val (MA1_Ptr.Second_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Second_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Second_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Second_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Second_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Second_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Second_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Second_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Second_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Second_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Second_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Second_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Second_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Second_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Second_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Second_64;

   procedure Test_Square_Meter_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("SquareMeter64");
      Print ("MemberId: ", Dots_Test.Member_Types.Square_Meter_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Square_Meter_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Square_Meter_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Square_Meter_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Square_Meter_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Square_Meter_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Square_Meter_64_Member.Is_Changed;
      MT1_Ptr.Square_Meter_64_Member.Set_Val (Dots_Test.Parameter_Types.Square_Meter_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Square_Meter_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Square_Meter_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Square_Meter_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Square_Meter_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Square_Meter_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("SquareMeter64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("SquareMeter64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("SquareMeter64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Square_Meter_64_Member (MT2_Smart_Ptr, MT1_Ptr.Square_Meter_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Square_Meter_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Square_Meter_64_Member (MI_Smart_Ptr, MT2_Ptr.Square_Meter_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Square_Meter_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Square_Meter_64_Member (MIA_Smart_Ptr, MT2_Ptr.Square_Meter_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Square_Meter_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Square_Meter_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Square_Meter_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Square_Meter_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Square_Meter_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Square_Meter_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Square_Meter_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Square_Meter_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Square_Meter_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Square_Meter_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Square_Meter_64_Member.Element (Ix).Set_Val (MA1_Ptr.Square_Meter_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Square_Meter_64_Member.Element (Ix).Set_Val (MA1_Ptr.Square_Meter_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Square_Meter_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Square_Meter_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Square_Meter_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Square_Meter_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Square_Meter_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Square_Meter_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Square_Meter_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Square_Meter_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Square_Meter_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Square_Meter_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Square_Meter_64;

   procedure Test_Steradian_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Steradian64");
      Print ("MemberId: ", Dots_Test.Member_Types.Steradian_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Steradian_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Steradian_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Steradian_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Steradian_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Steradian_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Steradian_64_Member.Is_Changed;
      MT1_Ptr.Steradian_64_Member.Set_Val (Dots_Test.Parameter_Types.Steradian_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Steradian_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Steradian_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Steradian_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Steradian_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Steradian_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Steradian64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Steradian64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Steradian64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Steradian_64_Member (MT2_Smart_Ptr, MT1_Ptr.Steradian_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Steradian_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Steradian_64_Member (MI_Smart_Ptr, MT2_Ptr.Steradian_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Steradian_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Steradian_64_Member (MIA_Smart_Ptr, MT2_Ptr.Steradian_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Steradian_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Steradian_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Steradian_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Steradian_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Steradian_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Steradian_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Steradian_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Steradian_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Steradian_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Steradian_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Steradian_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Steradian_64_Member.Element (Ix).Set_Val (MA1_Ptr.Steradian_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Steradian_64_Member.Element (Ix).Set_Val (MA1_Ptr.Steradian_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Steradian_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Steradian_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Steradian_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Steradian_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Steradian_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Steradian_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Steradian_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Steradian_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Steradian_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Steradian_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Steradian_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Steradian_64;

   procedure Test_Volt_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Volt64");
      Print ("MemberId: ", Dots_Test.Member_Types.Volt_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Volt_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Volt_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Volt_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Volt_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Volt_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Volt_64_Member.Is_Changed;
      MT1_Ptr.Volt_64_Member.Set_Val (Dots_Test.Parameter_Types.Volt_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Volt_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Volt_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Volt_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Volt_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Volt_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Volt64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Volt64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Volt64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Volt_64_Member (MT2_Smart_Ptr, MT1_Ptr.Volt_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Volt_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Volt_64_Member (MI_Smart_Ptr, MT2_Ptr.Volt_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Volt_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Volt_64_Member (MIA_Smart_Ptr, MT2_Ptr.Volt_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Volt_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Volt_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Volt_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Volt_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Volt_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Volt_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Volt_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Volt_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Volt_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Volt_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Volt_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Volt_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Volt_64_Member.Element (Ix).Set_Val (MA1_Ptr.Volt_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Volt_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Volt_64_Member.Element (Ix).Set_Val (MA1_Ptr.Volt_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Volt_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Volt_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Volt_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Volt_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Volt_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Volt_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Volt_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Volt_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Volt_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Volt_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Volt_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Volt_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Volt_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Volt_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Volt_64;

   procedure Test_Watt_64 is
      Null_Ok, In_Req_Ok : Boolean := True;

      L_MT               : Safir.Dob.Typesystem.Member_Type;
      L_MN               : Unbounded_Wide_String;
      L_MT_Id            : Safir.Dob.Typesystem.Type_Id;
      L_SL               : Safir.Dob.Typesystem.Int_32;
      L_IA               : Boolean;
      L_AL               : Safir.Dob.Typesystem.Int_32;

      Arrays_Item_Smart_Ptr : Dots_Test.Arrays_Item.Smart_Pointer;
      Arrays_Item_Ptr : Dots_Test.Arrays_Item.Arrays_Item_Class_Access := Arrays_Item_Smart_Ptr.Ref;
   begin
      --  1. Member Parameter -> Member Class MT1 -> Member Property MT2 -> Output
      --  2. Member Property MT2 -> Member Item MI -> Output
      --  3. Member Property MT2 -> Member Item Array MIA -> Member Type Property MIA -> Output
      --  4. Member Property EO -> Output
      --  5. Member Parameter -> Member Array MA1 -> Member Array MA2 -> Member Array Property MA2 -> Output
      --  6. Member array Property EO -  > Output

      Header ("Watt64");
      Print ("MemberId: ", Dots_Test.Member_Types.Watt_64_Member_Member_Index);
      Print ("MemberId (arr): ", Dots_Test.Member_Arrays.Watt_64_Member_Member_Index);
      Print ("Array Size Ok: ",
             Dots_Test.Parameter_Arrays.Watt_64_Parameter_Array_Size = 2 and
               Dots_Test.Member_Arrays.Watt_64_Member_Array_Size = 2 and
            Dots_Test.Member_Arrays_Property.Watt_64_Member_Array_Size (MA1_Smart_Ptr) = 2);

      Null_Ok := MT1_Ptr.Watt_64_Member.Is_Null;
      In_Req_Ok := not MT1_Ptr.Watt_64_Member.Is_Changed;
      MT1_Ptr.Watt_64_Member.Set_Val (Dots_Test.Parameter_Types.Watt_64_Parameter);
      Null_Ok := Null_Ok and not MT1_Ptr.Watt_64_Member.Is_Null;
      In_Req_Ok := In_Req_Ok and MT1_Ptr.Watt_64_Member.Is_Changed;
      Safir.Dob.Typesystem.Members.Get_Info (Dots_Test.Member_Arrays.Class_Type_Id,
                                             Dots_Test.Member_Arrays.Watt_64_Member_Member_Index,
                                             L_MT,
                                             L_MN,
                                             L_MT_Id,
                                             L_SL,
                                             L_IA,
                                             L_AL);

      Print ("----Members---- ");
      Print ("GetInfo: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (L_MT), '_') & ','
        & Safir.Dob.Typesystem.Utilities.To_Utf_8 (L_MN) & ','
        & Trim (Safir.Dob.Typesystem.Type_Id'Image (L_MT_Id)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_SL)) & ','
        & To_Lower (Boolean'Image (L_IA)) & ','
        & Trim (Safir.Dob.Typesystem.Int_32'Image (L_AL)));
      Print ("GetName: ", Safir.Dob.Typesystem.Members.Get_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Watt_64_Member_Member_Index));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Members.Get_Type_Name (Dots_Test.Member_Types.Class_Type_Id,
        Dots_Test.Member_Types.Watt_64_Member_Member_Index));

      Print ("----Parameters---- ");
      Print ("GetName: ", Safir.Dob.Typesystem.Parameters.Get_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Watt64Parameter"))));
      Print ("GetType: ", Strip (Safir.Dob.Typesystem.Member_Type'Image (Safir.Dob.Typesystem.Parameters.Get_Type
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Watt64Parameter")))), '_'));
      Print ("GetTypeName: ", Safir.Dob.Typesystem.Parameters.Get_Type_Name
        (Dots_Test.Parameter_Types.Class_Type_Id,
           Safir.Dob.Typesystem.Parameters.Get_Index
             (Dots_Test.Parameter_Types.Class_Type_Id, To_Unbounded_Wide_String ("Watt64Parameter"))));

      Print ("------------------ ");

      -- MemberTypes
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (MT2_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Watt_64_Member (MT2_Smart_Ptr, MT1_Ptr.Watt_64_Member.Get_Val);
      Print ("Val: ", Dots_Test.Member_Types_Property.Get_Watt_64_Member (MT2_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (MT2_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (MT2_Smart_Ptr);

      -- MemberItems
      MI_Ptr.Types_Item.Set_Ptr (Dots_Test.Types_Item.Create);
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (MI_Smart_Ptr);
      Dots_Test.Member_Types_Property.Set_Watt_64_Member (MI_Smart_Ptr, MT2_Ptr.Watt_64_Member.Get_Val);
      Print ("Item Val: ", Dots_Test.Member_Types_Property.Get_Watt_64_Member (MI_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (MI_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (MI_Smart_Ptr);

      -- MemberItemsArray
      Null_Ok := Null_Ok and Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (MIA_Smart_Ptr);
      MIA_Ptr.Types_Item_Array.Element (1).Set_Ptr (Dots_Test.Types_Item.Create);
      Dots_Test.Member_Types_Property.Set_Watt_64_Member (MIA_Smart_Ptr, MT2_Ptr.Watt_64_Member.Get_Val);
      Print ("Item Array Val: ", MIA_Ptr.Types_Item_Array.Element (1).Get_Ptr.Ref.Watt_64_Member.Get_Val);
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (MIA_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (MIA_Smart_Ptr);

      -- EmptyObject
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (EO_Smart_Ptr);
      Print ("Property Parameter Val: ", Dots_Test.Member_Types_Property.Get_Watt_64_Member (EO_Smart_Ptr));
      Null_Ok := Null_Ok and not Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (EO_Smart_Ptr);
      In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Types_Property.Is_Changed_Watt_64_Member (EO_Smart_Ptr);

      -- Array test
      for Ix in Safir.Dob.Typesystem.Array_Index range 0 .. Dots_Test.Parameter_Arrays.Watt_64_Parameter_Array_Size - 1 loop

         --MemberArray
         Null_Ok := Null_Ok and MA1_Ptr.Watt_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and not MA1_Ptr.Watt_64_Member.Element (Ix).Is_Changed;
         MA1_Ptr.Watt_64_Member.Element (Ix).Set_Val (Dots_Test.Parameter_Arrays.Watt_64_Parameter (Ix));
         Null_Ok := Null_Ok and not MA1_Ptr.Watt_64_Member.Element (Ix).Is_Null;
         In_Req_Ok := In_Req_Ok and MA1_Ptr.Watt_64_Member.Element (Ix).Is_Changed;

         -- MemberArray
         Null_Ok := Null_Ok and Dots_Test.Member_Arrays_Property.Is_Null_Watt_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Watt_64_Member (MA2_Smart_Ptr, Ix);
         MA2_Ptr.Watt_64_Member.Element (Ix).Set_Val (MA1_Ptr.Watt_64_Member.Element (Ix).Get_Val);
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_64_Member (MA2_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Watt_64_Member (MA2_Smart_Ptr, Ix);

         Print ("Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_64_Member (MA2_Smart_Ptr, Ix));

         -- Member Item
         Arrays_Item_Smart_Ptr := Dots_Test.Arrays_Item.Create;
         Arrays_Item_Ptr := Arrays_Item_Smart_Ptr.Ref;
         Arrays_Item_Ptr.Watt_64_Member.Element (Ix).Set_Val (MA1_Ptr.Watt_64_Member.Element (Ix).Get_Val);

         MI_Ptr.Arrays_Item.Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_64_Member (MI_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_64_Member (MI_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Watt_64_Member (MI_Smart_Ptr, Ix);

         -- Member Item Array
         MIA_Ptr.Arrays_Item_Array.Element (1).Set_Ptr (Arrays_Item_Smart_Ptr);
         Print ("Array Item Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_64_Member (MIA_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_64_Member (MIA_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and Dots_Test.Member_Arrays_Property.Is_Changed_Watt_64_Member (MIA_Smart_Ptr, Ix);

         --  EmptyObject
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Watt_64_Member (EO_Smart_Ptr, Ix);
         Print ("Parameter Array Val " & Trim (Safir.Dob.Typesystem.Int_32'Image (Ix)) & ": ", Dots_Test.Member_Arrays_Property.Get_Watt_64_Member (EO_Smart_Ptr, Ix));
         Null_Ok := Null_Ok and not Dots_Test.Member_Arrays_Property.Is_Null_Watt_64_Member (EO_Smart_Ptr, Ix);
         In_Req_Ok := In_Req_Ok and not Dots_Test.Member_Arrays_Property.Is_Changed_Watt_64_Member (EO_Smart_Ptr, Ix);
      end loop;

      -- SetNull Test
      MT1_Ptr.Watt_64_Member.Set_Null;
      Dots_Test.Member_Types_Property.Set_Null_Watt_64_Member (MT2_Smart_Ptr);
      MA1_Ptr.Watt_64_Member.Element (1).Set_Null;
      Dots_Test.Member_Arrays_Property.Set_Null_Watt_64_Member (MA2_Smart_Ptr, 1);
      Null_Ok := Null_Ok and
        MT1_Ptr.Watt_64_Member.Is_Null and
        Dots_Test.Member_Types_Property.Is_Null_Watt_64_Member (MT2_Smart_Ptr) and
        MA1_Ptr.Watt_64_Member.Element (1).Is_Null and
        Dots_Test.Member_Arrays_Property.Is_Null_Watt_64_Member (MA2_Smart_Ptr, 1);

      Print ("Is_Null OK: ", Null_Ok);
      Print ("Is_Changed OK: ", In_Req_Ok);

   end Test_Watt_64;

   procedure Test_Exception is

   begin
      Header ("TestException");
      begin
         raise Dots_Test.Test_Exception.Xception;
      exception
         when Dots_Test.Test_Exception.Xception =>
            Print ("Caught exception: DotsTest.TestException");
      end;
   end Test_Exception;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String)
                    renames  Ada.Exceptions.Raise_Exception;

   procedure Test_Library_Exceptions is
   begin
      Header ("LibraryExceptions");
      begin
         Throw (Dots_Test.Test_Exception.Xception'Identity, "Something has gone wrong");
      exception
         when E : others =>
            Safir.Dob.Typesystem.Library_Exceptions.Set (E);
      end;
      begin
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      exception
         when Dots_Test.Test_Exception.Xception =>
            Put_Line ("Caught exception: DotsTest.TestException");
      end;

      begin
         Throw (Constraint_Error'Identity, "Bla");
      exception
         when E : others =>
            Safir.Dob.Typesystem.Library_Exceptions.Set (E);
      end;
      begin
         Safir.Dob.Typesystem.Library_Exceptions.Throw;
      exception
         when Program_Error =>
            Put_Line ("Caught native exception");
      end;

   end Test_Library_Exceptions;


begin

   MA1_Smart_Ptr := Dots_Test.Member_Arrays.Create;
   MA1_Ptr := MA1_Smart_Ptr.Ref;
   MA2_Smart_Ptr := Dots_Test.Member_Arrays.Create;
   MA2_Ptr := MA2_Smart_Ptr.Ref;

   MT1_Smart_Ptr := Dots_Test.Member_Types.Create;
   MT1_Ptr := MT1_Smart_Ptr.Ref;
   MT2_Smart_Ptr := Dots_Test.Member_Types.Create;
   MT2_Ptr := MT2_Smart_Ptr.Ref;

   MI_Smart_Ptr := Dots_Test.Member_Items.Create;
   MI_Ptr := MI_Smart_Ptr.Ref;

   MIA_Smart_Ptr := Dots_Test.Member_Items_Array.Create;
   MIA_Ptr := MIA_Smart_Ptr.Ref;

   EO_Smart_Ptr := Dots_Test.Empty_Object.Create;
   --EO_Ptr := EO_Smart_Ptr.Ref;

   --Test_Class;
   --Test_Prop;

   Test_Has_Property;
   Test_Get_Name;
   Test_Get_Number_Of_Members;
   Test_Get_Number_Of_Parameters;
   Test_Create_Routines;

   Test_Int_32;
   Test_Int_64;
   Test_Float_32;
   Test_Float_64;
   Test_Boolean;
   Test_Enumeration;
   Test_String;
   Test_Entity_Id;
   Test_Instance_Id;
   Test_Type_Id;
   Test_Channel_Id;
   Test_Handler_Id;
   Test_Object;
   Test_Binary;
   Test_Test_Class;
   Test_Ampere_32;
   Test_Cubic_Meter_32;
   Test_Hertz_32;
   Test_Joule_32;
   Test_Kelvin_32;
   Test_Kilogram_32;
   Test_Meter_32;
   Test_Meter_Per_Second_32;
   Test_Meter_Per_Second_Squared_32;
   Test_Newton_32;
   Test_Pascal_32;
   Test_Radian_32;
   Test_Radian_Per_Second_32;
   Test_Radian_Per_Second_Squared_32;
   Test_Second_32;
   Test_Square_Meter_32;
   Test_Steradian_32;
   Test_Volt_32;
   Test_Watt_32;
   Test_Ampere_64;
   Test_Cubic_Meter_64;
   Test_Hertz_64;
   Test_Joule_64;
   Test_Kelvin_64;
   Test_Kilogram_64;
   Test_Meter_64;
   Test_Meter_Per_Second_64;
   Test_Meter_Per_Second_Squared_64;
   Test_Newton_64;
   Test_Pascal_64;
   Test_Radian_64;
   Test_Radian_Per_Second_64;
   Test_Radian_Per_Second_Squared_64;
   Test_Second_64;
   Test_Square_Meter_64;
   Test_Steradian_64;
   Test_Volt_64;
   Test_Watt_64;

   Test_Exception;
   Test_Library_Exceptions;

exception
   when E : others =>
      Put_Line ("Caught Exception, message: " & Ada.Exceptions.Exception_Information (E));
      raise;
end Dots_Test_Ada;
