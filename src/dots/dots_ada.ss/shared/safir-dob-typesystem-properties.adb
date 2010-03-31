-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009 (http://www.safirsdk.com)
--
--  Created by: Anders Widén / stawi
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
with Safir.Dob.Typesystem.Internal_Defs; use Safir.Dob.Typesystem.Internal_Defs;
with Safir.Dob.Typesystem.Container_Base;
with Safir.Dob.Typesystem.Object_Container_Base;
with Safir.Dob.Typesystem.Container_Instantiations; use Safir.Dob.Typesystem.Container_Instantiations;
with Safir.Dob.Typesystem.Enumeration_Container_Base;
with Safir.Dob.Typesystem.String_Container;
with Safir.Dob.Typesystem.Binary_Container;
with Safir.Dob.Typesystem.Object_Factory;
with Safir.Dob.Typesystem.Kernel;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Properties is

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   package C renames Interfaces.C;

   function To_Int_8 is new Ada.Unchecked_Conversion (C.char,
                                                      Safir.Dob.Typesystem.Int_8);

   function Get_Property_Mapping_Kind
     (Type_Id     : in Safir.Dob.Typesystem.Type_Id;
      Property_Id : in Safir.Dob.Typesystem.Type_Id;
      Member      : in Safir.Dob.Typesystem.Member_Index)
      return Safir.Dob.Typesystem.Internal_Defs.DotsC_Property_Mapping_Kind is

      Kind : Safir.Dob.Typesystem.Internal_Defs.DotsC_Property_Mapping_Kind;
      Error_Code : Safir.Dob.Typesystem.Internal_Defs.DotsC_Error_Code;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Property_Mapping_Kind
        (Type_Id, Property_Id, Member, Kind, Error_Code);

      case Error_Code is
         when Safir.Dob.Typesystem.Internal_Defs.No_Error =>
            null;
         when Safir.Dob.Typesystem.Internal_Defs.Illegal_Value =>
            Throw (Illegal_Value_Exception'Identity,
                   "That obj is not mapped to that property!");
         when Safir.Dob.Typesystem.Internal_Defs.Read_Only_Property |
              Safir.Dob.Typesystem.Internal_Defs.Unable_To_Dereference_Property =>
            Throw (Software_Violation_Exception'Identity,
                   "Got unexpected error code from dots_kernel");
      end case;

      return Kind;

   end Get_Property_Mapping_Kind;

   procedure Dereference_Class_Member_Reference (Object_Ptr        : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                                                 Class_Member_Ref  : in out Int_Ptrs.Pointer;
                                                 Ref_Size          : in Safir.Dob.Typesystem.Int_32;
                                                 Index             : in Safir.Dob.Typesystem.Array_Index;
                                                 Container         : out Safir.Dob.Typesystem.Container_Base.Container_Base_Access;
                                                 Parent_Is_Changed : in out Boolean) is
      use type Int_Ptrs.Pointer;

      Member_Index : Safir.Dob.Typesystem.Int_32;
      Array_Index  : Safir.Dob.Typesystem.Int_32;

      Member : Safir.Dob.Typesystem.Container_Base.Container_Base_Access;

   begin
      Member_Index := Class_Member_Ref.all;
      Int_Ptrs.Increment (Class_Member_Ref);
      Array_Index := Class_Member_Ref.all;
      Int_Ptrs.Increment (Class_Member_Ref);

      if Ref_Size > 2 then  -- we need to recurse into child objects

         Member := Object_Ptr.Ref.Get_Member (Member_Index, Array_Index);

         if Member.Is_Changed then
            Parent_Is_Changed := True;
         end if;

         if Member.Is_Null then
            Container := null;
         else
            Dereference_Class_Member_Reference
              (Safir.Dob.Typesystem.Object_Container_Base.
                 Object_Container_Base_Access (Member).Get_Object_Pointer.all,
               Class_Member_Ref,
               Ref_Size - 2,
               Index,
               Container,
               Parent_Is_Changed);
         end if;

      else
         if Array_Index = -1 then -- pointing at an array, use the index from the procedure call
            Container := Object_Ptr.Ref.Get_Member (Member_Index, Index);
         else
            if Index /= 0 then
               Throw (Software_Violation_Exception'Identity, "CMR says that the member is not an array, but I got passed an index /= 0");
            end if;
            Container := Object_Ptr.Ref.Get_Member (Member_Index, Array_Index);
         end if;

      end if;

   end Dereference_Class_Member_Reference;


   procedure Get_Member_Container (Obj_Ptr           : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                                   Property_Id       : in Safir.Dob.Typesystem.Type_Id;
                                   Member            : in Safir.Dob.Typesystem.Member_Index;
                                   Index             : in Safir.Dob.Typesystem.Array_Index;
                                   Container         : out Safir.Dob.Typesystem.Container_Base.Container_Base_Access;
                                   Parent_Is_Changed : in out Boolean) is

      use type Int_Ptrs.Pointer;

      Class_Member_Ref : Int_Ptrs.Pointer;
      Ref_Size : Safir.Dob.Typesystem.Int_32;

      Type_Id : constant Safir.Dob.Typesystem.Type_Id := Obj_Ptr.Ref.Get_Type_Id;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Class_Member_Reference
        (Type_Id, Property_Id, Member, Class_Member_Ref, Ref_Size);

      if Class_Member_Ref = null or Ref_Size = 0 then
         Throw (Software_Violation_Exception'Identity, "Failed to get class member reference from dots_kernel");
      end if;

      Dereference_Class_Member_Reference (Obj_Ptr,
                                          Class_Member_Ref,
                                          Ref_Size,
                                          Index,
                                          Container,
                                          Parent_Is_Changed);

   end Get_Member_Container;

   --------------------
   -- Get_Array_Size --
   --------------------
   function Get_Array_Size
     (Class_Id        : Safir.Dob.Typesystem.Type_Id;
      Property_Id     : Safir.Dob.Typesystem.Type_Id;
      Member          : Safir.Dob.Typesystem.Member_Index)
      return Safir.Dob.Typesystem.Int_32 is

      Result : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_Member_Array_Size_Property
        (Class_Id, Property_Id, Member);
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "No such type or array or mapping defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Class_Id)
                & " - " & Safir.Dob.Typesystem.Member_Index'Image (Member)
                & " - " & Safir.Dob.Typesystem.Type_Id'Image (Property_Id) & ")");
      end if;
      return Result;
   end Get_Array_Size;

   --------------------------
   -- Get_String_Max_Length --
   ---------------------------
   function Get_String_Max_Length
     (Class_Id        : Safir.Dob.Typesystem.Type_Id;
      Property_Id     : Safir.Dob.Typesystem.Type_Id;
      Member          : Safir.Dob.Typesystem.Member_Index)
      return Safir.Dob.Typesystem.Int_32 is

      Result : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_String_Member_Max_Length_Property
        (Class_Id, Property_Id, Member);
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "No such type or mapping defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Class_Id)
                & " - " & Safir.Dob.Typesystem.Member_Index'Image (Member)
                & " - " & Safir.Dob.Typesystem.Type_Id'Image (Property_Id) & ")");
      end if;
      return Result;
   end Get_String_Max_Length;

   --------------
   -- Set_Null --
   --------------
   procedure Set_Null (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                       Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                       Member             : in Safir.Dob.Typesystem.Member_Index;
                       Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Base.Container_Base_Access;

      Container : Safir.Dob.Typesystem.Container_Base.Container_Base_Access;
      Parent_Is_Changed : Boolean := False;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            return;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Container,
                                  Parent_Is_Changed);

            if Container = null then
               return; -- parent is null
            end if;

            Container.Set_Null;

      end case;
   end Set_Null;

   -------------
   -- Is_Null --
   -------------
   function Is_Null (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                     Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                     Member             : in Safir.Dob.Typesystem.Member_Index;
                     Index              : in Safir.Dob.Typesystem.Array_Index)
                     return Boolean is
      use type Safir.Dob.Typesystem.Container_Base.Container_Base_Access;

      Container : Safir.Dob.Typesystem.Container_Base.Container_Base_Access;
      Parent_Is_Changed : Boolean := False;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            return True;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            return False;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Container,
                                  Parent_Is_Changed);

            if Container = null or else Container.Is_Null then
               return True;  -- parent or container is null
            end if;

            return False;

      end case;
   end Is_Null;

   ----------------
   -- Is_Changed --
   ----------------
   function Is_Changed (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                        Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                        Member             : in Safir.Dob.Typesystem.Member_Index;
                        Index              : in Safir.Dob.Typesystem.Array_Index)
                        return Boolean is
      use type Safir.Dob.Typesystem.Container_Base.Container_Base_Access;

      Container : Safir.Dob.Typesystem.Container_Base.Container_Base_Access;
      Parent_Is_Changed : Boolean := False;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            return False;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            return False;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Container,
                                  Parent_Is_Changed);

            if Parent_Is_Changed then
               return True;
            end if;

            if Container /= null and then Container.Is_Changed then
               return True;
            end if;

            return False;

      end case;
   end Is_Changed;

   ------------------
   -- Is_Read_Only --
   ------------------
   function Is_Read_Only (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                          Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                          Member             : in Safir.Dob.Typesystem.Member_Index;
                          Index              : in Safir.Dob.Typesystem.Array_Index)
                          return Boolean is
      use type Safir.Dob.Typesystem.Container_Base.Container_Base_Access;

      Container : Safir.Dob.Typesystem.Container_Base.Container_Base_Access;
      Parent_Is_Changed : Boolean := False;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            return True;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            return True;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Container,
                                  Parent_Is_Changed);

            if Container = null then
               return True;
            end if;

            return False;

      end case;
   end Is_Read_Only;

   ------------------
   -- Set (Boolean) --
   ------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Boolean;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Boolean_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Boolean_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   ------------------
   -- Get (Boolean) --
   ------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Boolean;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Boolean_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Boolean_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Val : C.char;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Boolean_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val);
            Value := C.char'Pos (Val) /= 0;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   --------------
   -- Set_Enum --
   --------------
   procedure Set_Enum (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                       Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                       Value              : in Safir.Dob.Typesystem.Enumeration_Value;
                       Member             : in Safir.Dob.Typesystem.Member_Index;
                       Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Enumeration_Container_Base.Enumeration_Container_Base_Access;

      Container : Safir.Dob.Typesystem.Enumeration_Container_Base.Enumeration_Container_Base_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Ordinal (Value);

      end case;
   end Set_Enum;

   --------------
   -- Get_Enum --
   --------------
   procedure Get_Enum (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                       Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                       Value              : out Safir.Dob.Typesystem.Enumeration_Value;
                       Member             : in Safir.Dob.Typesystem.Member_Index;
                       Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Enumeration_Container_Base.Enumeration_Container_Base_Access;

      Container : Safir.Dob.Typesystem.Enumeration_Container_Base.Enumeration_Container_Base_Access;
      Parent_Is_Changed : Boolean := False;

      Val : Safir.Dob.Typesystem.Int_32;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Enumeration_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val);
            Value := Val;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Ordinal;
      end case;
   end Get_Enum;

   ------------------
   -- Set (Int_32) --
   ------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Int_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Int_32_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Int_32_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   ------------------
   -- Get (Int_32) --
   ------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Int_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Int_32_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Int_32_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Val : Safir.Dob.Typesystem.Int_32;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Int_32_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val);
            Value := Val;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   ------------------
   -- Set (Int_64) --
   ------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Int_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Int_64_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Int_64_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   ------------------
   -- Get (Int_64) --
   ------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Int_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Int_64_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Int_64_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Val : Safir.Dob.Typesystem.Int_64;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Int_64_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val);
            Value := Val;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   --------------------
   -- Set (Float_32) --
   --------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Float_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Float_32_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Float_32_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   --------------------
   -- Get (Float_32) --
   --------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Float_32;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Float_32_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Float_32_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Val : Safir.Dob.Typesystem.Float_32;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Float_32_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val);
            Value := Val;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   --------------------
   -- Set (Float_64) --
   --------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Float_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Float_64_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Float_64_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   --------------------
   -- Get (Float_64) --
   --------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Float_64;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is

      use type Safir.Dob.Typesystem.Container_Instantiations.Float_64_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Float_64_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Val : Safir.Dob.Typesystem.Float_64;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Float_64_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val);
            Value := Val;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   -----------------------
   -- Set (Instance_Id) --
   -----------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Instance_Id_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Instance_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   -----------------------
   -- Get (Instance_Id) --
   -----------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Instance_Id_Container.Container_Access;
      use type C.Strings.chars_ptr;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Instance_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Hash_Val : Safir.Dob.Typesystem.Int_64;
      Str_Val : C.Strings.chars_ptr;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Hash_Val, Str_Val);

            if Str_Val = C.Strings.Null_Ptr then
               Value := Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Hash_Val);
            else
               Value := Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
                 (Hash_Val, From_Utf_8 (C.To_Ada (C.Strings.Value (Str_Val))));
            end if;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   ---------------------
   -- Set (Entity_Id) --
   ---------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Entity_Id_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Entity_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   ---------------------
   -- Get (Entity_Id) --
   ---------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Entity_Id_Container.Container_Access;
      use type C.Strings.chars_ptr;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Entity_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Entity_Id_Val : Safir.Dob.Typesystem.Internal_Defs.DotsC_Entity_Id;
      Str_Val : C.Strings.chars_ptr;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Entity_Id_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Entity_Id_Val, Str_Val);

            if Str_Val = C.Strings.Null_Ptr then
               Value := Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
                 (Entity_Id_Val.Type_Id,
                  Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (Entity_Id_Val.Instance_Id));
            else
               Value := Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
                 (Entity_Id_Val.Type_Id,
                  Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
                    (Entity_Id_Val.Instance_Id, From_Utf_8 (C.To_Ada (C.Strings.Value (Str_Val)))));
            end if;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   ----------------------
   -- Set (Channel_Id) --
   ----------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Channel_Id_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Channel_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   -----------------------
   -- Get (Channel_Id) --
   -----------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Channel_Id_Container.Container_Access;
      use type C.Strings.chars_ptr;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Channel_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Hash_Val : Safir.Dob.Typesystem.Int_64;
      Str_Val : C.Strings.chars_ptr;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Hash_Val, Str_Val);

            if Str_Val = C.Strings.Null_Ptr then
               Value := Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id (Hash_Val);
            else
               Value := Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id
                 (Hash_Val, From_Utf_8 (C.To_Ada (C.Strings.Value (Str_Val))));
            end if;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   ----------------------
   -- Set (Handler_Id) --
   ----------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Handler_Id_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Handler_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   ----------------------
   -- Get (Handler_id) --
   ----------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Container_Instantiations.Handler_Id_Container.Container_Access;
      use type C.Strings.chars_ptr;

      Container : Safir.Dob.Typesystem.Container_Instantiations.Handler_Id_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Hash_Val : Safir.Dob.Typesystem.Int_64;
      Str_Val : C.Strings.chars_ptr;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Hash_Val, Str_Val);

            if Str_Val = C.Strings.Null_Ptr then
               Value := Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (Hash_Val);
            else
               Value := Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
                 (Hash_Val, From_Utf_8 (C.To_Ada (C.Strings.Value (Str_Val))));
            end if;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   ------------------
   -- Set (string) --
   ------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Unbounded_Wide_String;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.String_Container.Container_Access;

      Container : Safir.Dob.Typesystem.String_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   ------------------
   -- Get (string) --
   ------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Unbounded_Wide_String;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.String_Container.Container_Access;
      use type C.Strings.chars_ptr;

      Container : Safir.Dob.Typesystem.String_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Val : C.Strings.chars_ptr;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_String_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val);
            Value := From_Utf_8 (C.To_Ada (C.Strings.Value (Val)));

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val;
      end case;
   end Get;

   ------------------
   -- Set (object) --
   ------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Ptr                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Access;

      Container : Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Ptr (Safir.Dob.Typesystem.Object.Smart_Pointer (Ptr));

      end case;
   end Set;

   ------------------
   -- Get (object) --
   ------------------
   function Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                 Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                 Member             : in Safir.Dob.Typesystem.Member_Index;
                 Index              : in Safir.Dob.Typesystem.Array_Index)
                 return Safir.Dob.Typesystem.Object.Smart_Pointer'Class is
      use type Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Access;

      Container : Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Access;
      Parent_Is_Changed : Boolean := False;

      Blob : Safir.Dob.Typesystem.Blob_T;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Object_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Blob);
            declare
               Ptr : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                       Safir.Dob.Typesystem.Object_Factory.Create_Object (Blob);
            begin
               -- A blob created from a parameter will have the change flags set
               -- so we must reset them.
               Ptr.Ref.Set_Changed (False);
               return Ptr;
            end;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            if Container.Is_Null then
               Throw (Null_Exception'Identity, "Object is null");
            end if;

            return Container.Get_Object_Pointer.all;

      end case;
   end Get;

--     procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
--                    Property_Id        : in Safir.Dob.Typesystem.Type_Id;
--                    Ptr                : out Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
--                    Member             : in Safir.Dob.Typesystem.Member_Index;
--                    Index              : in Safir.Dob.Typesystem.Array_Index) is
--        use type Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Access;
--
--        Container : Safir.Dob.Typesystem.Object_Container_Base.Object_Container_Base_Access;
--        Parent_Is_Changed : Boolean := False;
--
--        Blob : Safir.Dob.Typesystem.Blob_T;
--     begin
--        case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is
--
--           when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
--              Throw (Read_Only_Exception'Identity, "Property member is mapped to null");
--
--           when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
--              Safir.Dob.Typesystem.Kernel.Get_Object_Property_Parameter
--                (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Blob);
--              Ptr := Safir.Dob.Typesystem.Object_Factory.Create_Object (Blob);
--              -- A blob created from a parameter will have the change flags set
--              Ptr.Ref.Set_Changed (False);
--
--           when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
--              Get_Member_Container (Obj,
--                                    Property_Id,
--                                    Member,
--                                    Index,
--                                    Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
--                                    Parent_Is_Changed);
--
--              if Container = null then
--                 Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
--              end if;
--
--              if Container.Is_Null then
--                 Throw (Null_Exception'Identity, "Object is null");
--              end if;
--
--              declare
--                 Tmp_Ptr : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
--                             Container.Get_Object_Pointer.all;
--                 --T_P     :  constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class := Tmp_Ptr;
--              begin
--                 --Ptr := Container.Get_Object_Pointer.all;
--                 Put_Line (Expanded_Name (Tmp_Ptr'Tag));
--                 if Tmp_Ptr in Safir.Dob.Typesystem.Object.Smart_Pointer'Class then
--                    Put_Line ("OK");
--                 else
--                    Put_Line ("NOT OK");
--                 end if;
--
--                 Ptr := Tmp_Ptr;
--              end;
--        end case;
--     end Get;

   ------------------
   -- Set (binary) --
   ------------------
   procedure Set (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : in Safir.Dob.Typesystem.Binary_Vectors.Vector;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Binary_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Binary_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to parameter");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Container.Set_Val (Value);

      end case;
   end Set;

   ------------------
   -- Get (binary) --
   ------------------
   procedure Get (Obj                : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                  Property_Id        : in Safir.Dob.Typesystem.Type_Id;
                  Value              : out Safir.Dob.Typesystem.Binary_Vectors.Vector;
                  Member             : in Safir.Dob.Typesystem.Member_Index;
                  Index              : in Safir.Dob.Typesystem.Array_Index) is
      use type Safir.Dob.Typesystem.Binary_Container.Container_Access;

      Container : Safir.Dob.Typesystem.Binary_Container.Container_Access;
      Parent_Is_Changed : Boolean := False;

      Val : Char_Ptrs.Pointer;
      Size : Safir.Dob.Typesystem.Int_32;
   begin
      case Get_Property_Mapping_Kind (Obj.Ref.Get_Type_Id, Property_Id, Member) is

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Null =>
            Throw (Read_Only_Exception'Identity, "Property member is mapped to null");

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Parameter =>
            Safir.Dob.Typesystem.Kernel.Get_Binary_Property_Parameter
              (Obj.Ref.Get_Type_Id, Property_Id, Member, Index, Val, Size);
            Value.Clear;
            Value.Reserve_Capacity (Ada.Containers.Count_Type (Size));
            for I in 1 .. Size loop
               Value.Append (To_Int_8 (Val.all));
               Char_Ptrs.Increment (Val);
            end loop;

         when Safir.Dob.Typesystem.Internal_Defs.Mapped_To_Member =>
            Get_Member_Container (Obj,
                                  Property_Id,
                                  Member,
                                  Index,
                                  Safir.Dob.Typesystem.Container_Base.Container_Base_Access (Container),
                                  Parent_Is_Changed);

            if Container = null then
               Throw (Read_Only_Exception'Identity, "Unable to dereference property, some parent is null");
            end if;

            Value := Container.Get_Val.all;
      end case;
   end Get;

end Safir.Dob.Typesystem.Properties;
