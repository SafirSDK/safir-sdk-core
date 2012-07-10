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
with Ada.Unchecked_Conversion;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with Safir.Dob.Typesystem.Kernel;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Object;
with Safir.Dob.Typesystem.Object.Factory;
with Safir.Dob.Typesystem.Internal_Defs;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Blob_Operations is

   package C renames Interfaces.C;

   use type Blob_T;
   use type C.Strings.chars_ptr;

   function To_Int_8 is new Ada.Unchecked_Conversion (C.char, Safir.Dob.Typesystem.Int_8);
   function To_Char is new Ada.Unchecked_Conversion (Safir.Dob.Typesystem.Int_8, C.char);

   -- Convert a char pointer to an int pointer using pointer arithmetic.
   -- This is used instead of an Unchecked_Conversion, since that gives
   -- warnings about alignment on ARM arch.
   function To_Int_Ptr (Ptr : in Char_Ptrs.Pointer)
      return Int_Ptrs.Pointer is
      use Int_Ptrs;
   begin
      return null + Interfaces.C.ptrdiff_t (Integer (Ptr - null));

   end To_Int_Ptr;

   function Get_Type_Id (Blob   : in Safir.Dob.Typesystem.Blob_T)
                         return Safir.Dob.Typesystem.Type_Id is
   begin
      pragma Assert (Blob /= null, "Cannot get type id from NULL blob!");
      return Safir.Dob.Typesystem.Kernel.Get_Type_Id (Blob);
   end Get_Type_Id;

   -- ================
   -- Value operations
   -- ================
   procedure Set_Null (Blob   : in Safir.Dob.Typesystem.Blob_T;
                       Member : in Safir.Dob.Typesystem.Member_Index;
                       Index  : in Safir.Dob.Typesystem.Array_Index) is
   begin
      Safir.Dob.Typesystem.Kernel.Set_Null_Member (Blob, Member, Index);
   end Set_Null;

   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Boolean;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean) is
   begin
      Safir.Dob.Typesystem.Kernel.Set_Boolean_Member_In_Preallocated
        (C.char'Val (Boolean'Pos (Value)),
         C.char'Val (Boolean'Pos (Is_Null)),
         C.char'Val (Boolean'Pos (Is_Changed)),
         Blob,
         Member,
         Index);
   end Set;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Boolean;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Value : C.char;
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Boolean_Member
        (Blob, Member, Index, L_Value, L_Is_Null, L_Is_Changed);

      Value := C.char'Pos (L_Value) /= 0;
      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;
   end Get;

   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Int_32;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean) is
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Set_Int_32_Member_In_Preallocated
        (Value,
         C.char'Val (Boolean'Pos (Is_Null)),
         C.char'Val (Boolean'Pos (Is_Changed)),
         Blob,
         Member,
         Index);
   end Set;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Int_32;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Int_32_Member
        (Blob, Member, Index, Value, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

   end Get;

   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Int_64;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean) is
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Set_Int_64_Member_In_Preallocated
        (Value,
         C.char'Val (Boolean'Pos (Is_Null)),
         C.char'Val (Boolean'Pos (Is_Changed)),
         Blob,
         Member,
         Index);
   end Set;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Int_64;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Int_64_Member
        (Blob, Member, Index, Value, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

   end Get;

   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Float_32;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean) is
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Set_Float_32_Member_In_Preallocated
        (Value,
         C.char'Val (Boolean'Pos (Is_Null)),
         C.char'Val (Boolean'Pos (Is_Changed)),
         Blob,
         Member,
         Index);
   end Set;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Float_32;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Float_32_Member
        (Blob, Member, Index, Value, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

   end Get;

   procedure Set (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : in Safir.Dob.Typesystem.Float_64;
                  Is_Null                : in Boolean;
                  Is_Changed             : in Boolean) is
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Set_Float_64_Member_In_Preallocated
        (Value,
         C.char'Val (Boolean'Pos (Is_Null)),
         C.char'Val (Boolean'Pos (Is_Changed)),
         Blob,
         Member,
         Index);
   end Set;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Float_64;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Float_64_Member
        (Blob, Member, Index, Value, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

   end Get;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
      L_Hash_Val   : Safir.Dob.Typesystem.Int_64;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Member
        (Blob, Member, Index, L_Hash_Val, L_Str_Val, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

      if not Is_Null then
         if L_Str_Val /= C.Strings.Null_Ptr then
            Value := Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
              (L_Hash_Val, From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val))));
         else
            Value := Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
              (L_Hash_Val);
         end if;
      end if;
   end Get;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
      L_Entity_Id_Val : Safir.Dob.Typesystem.Internal_Defs.DotsC_Entity_Id;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Entity_Id_Member
        (Blob, Member, Index, L_Entity_Id_Val, L_Str_Val, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

      if not Is_Null then
         if L_Str_Val /= C.Strings.Null_Ptr then
            Value := Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
              (L_Entity_Id_Val.Type_Id,
               Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
                 (L_Entity_Id_Val.Instance_Id, From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val)))));
         else
            Value := Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
              (L_Entity_Id_Val.Type_Id,
               Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (L_Entity_Id_Val.Instance_Id));
         end if;
      end if;
   end Get;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
      L_Hash_Val   : Safir.Dob.Typesystem.Int_64;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Member
        (Blob, Member, Index, L_Hash_Val, L_Str_Val, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

      if not Is_Null then
         if L_Str_Val /= C.Strings.Null_Ptr then
            Value := Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id
              (L_Hash_Val, From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val))));
         else
            Value := Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id
              (L_Hash_Val);
         end if;
      end if;
   end Get;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
      L_Hash_Val   : Safir.Dob.Typesystem.Int_64;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Member
        (Blob, Member, Index, L_Hash_Val, L_Str_Val, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

      if not Is_Null then
         if L_Str_Val /= C.Strings.Null_Ptr then
            Value := Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
              (L_Hash_Val, From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val))));
         else
            Value := Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
              (L_Hash_Val);
         end if;
      end if;
   end Get;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Unbounded_Wide_String;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_String_Member
        (Blob, Member, Index, L_Str_Val, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

      if not Is_Null then
         Value := From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val)));
      end if;
   end Get;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Blob_T;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Object_Member
        (Blob, Member, Index, Value, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;
   end Get;

   procedure Get (Blob                   : in Safir.Dob.Typesystem.Blob_T;
                  Member                 : in Safir.Dob.Typesystem.Member_Index;
                  Index                  : in Safir.Dob.Typesystem.Array_Index;
                  Value                  : out Safir.Dob.Typesystem.Binary_Vectors.Vector;
                  Is_Null                : out Boolean;
                  Is_Changed             : out Boolean) is
      L_Is_Null     : C.char;
      L_Is_Changed  : C.char;
      L_Value       : Safir.Dob.Typesystem.Char_Star;
      L_Size        : Safir.Dob.Typesystem.Int_32;
   begin
      pragma Assert (Blob /= null, "NULL Blob!");

      Safir.Dob.Typesystem.Kernel.Get_Binary_Member
        (Blob, Member, Index, L_Value, L_Size, L_Is_Null, L_Is_Changed);

      Is_Null := C.char'Pos (L_Is_Null) /= 0;
      Is_Changed := C.char'Pos (L_Is_Changed) /= 0;

      if not Is_Null then
         Value.Clear;
         Value.Reserve_Capacity (Ada.Containers.Count_Type (L_Size));
         for I in 1 .. L_Size loop
            Value.Append (To_Int_8 (L_Value.all));
            L_Value := L_Value + 1;
         end loop;
      end if;

   end Get;

   -- ====================
   -- Container operations
   -- ====================
   procedure Set (Value               : in Boolean_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Val : Boolean := False;
   begin
      if not Value.Is_Null then
         Val := Value.Get_Val;
      end if;
      Set (Blob, Member, Array_Index, Val, Value.Is_Null, Value.Is_Changed);
   end Set;

   procedure Get (Value               : out Boolean_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Boolean;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Boolean_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Enumeration_Container_Base.Enumeration_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Val : Safir.Dob.Typesystem.Int_32 := 0;
   begin
      if not Value.Is_Null then
         Val := Safir.Dob.Typesystem.Int_32 (Value.Get_Ordinal);
      end if;
      Set (Blob, Member, Array_Index, Val, Value.Is_Null, Value.Is_Changed);
   end Set;

   procedure Get (Value               : out Enumeration_Container_Base.Enumeration_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Int_32;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      if L_Is_Null then
         -- Must make this check because the Int_32 can have a
         -- "garbage" value after the call to Get if we are facing
         -- a null value, and it isn't sure that this garbage value
         -- will fit in the enumeration subtype
         L_Val := Safir.Dob.Typesystem.Enumeration_Value'First;
      end if;

      Value.Init (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Int_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Val : Safir.Dob.Typesystem.Int_32 := 0;
   begin
      if not Value.Is_Null then
         Val := Value.Get_Val;
      end if;
      Set (Blob, Member, Array_Index, Val, Value.Is_Null, Value.Is_Changed);
   end Set;

   procedure Get (Value               : out Int_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is

      L_Val : Safir.Dob.Typesystem.Int_32;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Int_32_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Int_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Val : Safir.Dob.Typesystem.Int_64 := 0;
   begin
      if not Value.Is_Null then
         Val := Value.Get_Val;
      end if;
      Set (Blob, Member, Array_Index, Val, Value.Is_Null, Value.Is_Changed);
   end Set;

   procedure Get (Value               : out Int_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Int_64;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Int_64_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Float_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Val : Safir.Dob.Typesystem.Float_32 := 0.0;
   begin
      if not Value.Is_Null then
         Val := Value.Get_Val;
      end if;
      Set (Blob, Member, Array_Index, Val, Value.Is_Null, Value.Is_Changed);
   end Set;

   procedure Get (Value               : out Float_32_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Float_32;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Float_32_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Float_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Val : Safir.Dob.Typesystem.Float_64 := 0.0;
   begin
      if not Value.Is_Null then
         Val := Value.Get_Val;
      end if;
      Set (Blob, Member, Array_Index, Val, Value.Is_Null, Value.Is_Changed);
   end Set;

   procedure Get (Value               : out Float_64_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Float_64;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Float_64_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Instance_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      use Safir.Dob.Typesystem.Instance_Id;

      String_Length        : Safir.Dob.Typesystem.Int_32;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      if not Value.Is_Null then
         String_Length := Utf_8_String_Length (Value.Get_Val);
         if String_Length > 0 then
            Utf_8_Str_Ptr :=
              C.Strings.New_String (To_String (Utf_8_String (Value.Get_Val)));
         end if;
         Safir.Dob.Typesystem.Kernel.Set_Hashed_Id_Member_In_Preallocated
           (Get_Raw_Value (Value.Get_Val),
            Utf_8_Str_Ptr,
            String_Length,
            C.char'Val (Boolean'Pos (Value.Is_Null)),
            C.char'Val (Boolean'Pos (Value.Is_Changed)),
            Blob,
            Member,
            Array_Index,
            Beginning_Of_Unused);
         C.Strings.Free (Utf_8_Str_Ptr);
      elsif Value.Is_Changed then
         Safir.Dob.Typesystem.Kernel.Set_Null_Member (Blob, Member, Array_Index);
      end if;
   end Set;

   procedure Get (Value               : out Instance_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Instance_Id_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Entity_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      use Safir.Dob.Typesystem.Entity_Id;
      use Safir.Dob.Typesystem.Instance_Id;

      String_Length        : Safir.Dob.Typesystem.Int_32;
      Underlying_Entity_Id : Safir.Dob.Typesystem.Kernel.Underlying_Entity_Id_Type;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      if not Value.Is_Null then
         String_Length := Utf_8_String_Length (Get_Instance_Id (Value.Get_Val));
         if String_Length > 0 then
            Utf_8_Str_Ptr :=
              C.Strings.New_String (To_String (Utf_8_String (Get_Instance_Id (Value.Get_Val))));
         end if;

         Underlying_Entity_Id := (Type_Id  => Get_Type_Id (Value.Get_Val),
                                  Instance => Get_Raw_Value (Get_Instance_Id (Value.Get_Val)));
         Safir.Dob.Typesystem.Kernel.Set_Entity_Id_Member_In_Preallocated
           (Underlying_Entity_Id,
            Utf_8_Str_Ptr,
            String_Length,
            C.char'Val (Boolean'Pos (Value.Is_Null)),
            C.char'Val (Boolean'Pos (Value.Is_Changed)),
            Blob,
            Member,
            Array_Index,
            Beginning_Of_Unused);
         C.Strings.Free (Utf_8_Str_Ptr);
      elsif Value.Is_Changed then
         Set_Null (Blob, Member, Array_Index);
      end if;
   end Set;

   procedure Get (Value               : out Entity_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Entity_Id_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Channel_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      use Safir.Dob.Typesystem.Channel_Id;

      String_Length        : Safir.Dob.Typesystem.Int_32;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      if not Value.Is_Null then
         String_Length := Utf_8_String_Length (Value.Get_Val);
         if String_Length > 0 then
            Utf_8_Str_Ptr :=
              C.Strings.New_String (To_String (Utf_8_String (Value.Get_Val)));
         end if;
         Safir.Dob.Typesystem.Kernel.Set_Hashed_Id_Member_In_Preallocated
           (Get_Raw_Value (Value.Get_Val),
            Utf_8_Str_Ptr,
            String_Length,
            C.char'Val (Boolean'Pos (Value.Is_Null)),
            C.char'Val (Boolean'Pos (Value.Is_Changed)),
            Blob,
            Member,
            Array_Index,
            Beginning_Of_Unused);
         C.Strings.Free (Utf_8_Str_Ptr);
      elsif Value.Is_Changed then
         Safir.Dob.Typesystem.Kernel.Set_Null_Member (Blob, Member, Array_Index);
      end if;
   end Set;

   procedure Get (Value               : out Channel_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Channel_Id_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Handler_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      use Safir.Dob.Typesystem.Handler_Id;

      String_Length        : Safir.Dob.Typesystem.Int_32;
      Utf_8_Str_Ptr : C.Strings.chars_ptr;
   begin
      if not Value.Is_Null then
         String_Length := Utf_8_String_Length (Value.Get_Val);
         if String_Length > 0 then
            Utf_8_Str_Ptr :=
              C.Strings.New_String (To_String (Utf_8_String (Value.Get_Val)));
         end if;
         Safir.Dob.Typesystem.Kernel.Set_Hashed_Id_Member_In_Preallocated
           (Get_Raw_Value (Value.Get_Val),
            Utf_8_Str_Ptr,
            String_Length,
            C.char'Val (Boolean'Pos (Value.Is_Null)),
            C.char'Val (Boolean'Pos (Value.Is_Changed)),
            Blob,
            Member,
            Array_Index,
            Beginning_Of_Unused);
         C.Strings.Free (Utf_8_Str_Ptr);
      elsif Value.Is_Changed then
         Safir.Dob.Typesystem.Kernel.Set_Null_Member (Blob, Member, Array_Index);
      end if;
   end Set;

   procedure Get (Value               : out Handler_Id_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Handler_Id_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in String_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is

      String_Start : Safir.Dob.Typesystem.Blob_T;
      String_Length : Safir.Dob.Typesystem.Int_32;
   begin
      if not Value.Is_Null then
         String_Start := Beginning_Of_Unused;
         String_Length := Value.Utf_8_String_Length;

         Safir.Dob.Typesystem.Kernel.Create_String_Member
           (Blob,
            String_Length,
            Member,
            Array_Index,
            C.char'Val (Boolean'Pos (Value.Is_Changed)),
            Beginning_Of_Unused);

         Safir.Dob.Typesystem.Utilities.Copy
           (Destination => String_Start,
            Source => C.To_C (To_String (Value.Utf_8_String), Append_Nul => True));

      elsif Value.Is_Changed then
         Set_Null (Blob, Member, Array_Index);
      end if;
   end Set;

   procedure Get (Value               : out String_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Unbounded_Wide_String;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := String_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   procedure Set (Value               : in Object_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is

      Child_Blob : constant Safir.Dob.Typesystem.Blob_T := Beginning_Of_Unused;
      Obj_Ptr : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                  Value.Get_Object_Pointer.all;
   begin
      if not Value.Is_Null then
         Safir.Dob.Typesystem.Kernel.Create_Object_Member
           (Blob,
            Obj_Ptr.Ref.Calculate_Blob_Size,
            Obj_Ptr.Ref.Get_Type_Id,
            Member,
            Array_Index,
            C.char'Val (Boolean'Pos (Value.Is_Changed_Here)),
            Beginning_Of_Unused);
         Obj_Ptr.Ref.Write_To_Blob (Child_Blob, Beginning_Of_Unused);
      elsif Value.Is_Changed_Here then
         Safir.Dob.Typesystem.Kernel.Set_Null_Member (Blob, Member, Array_Index);
      end if;

   end Set;

   procedure Get (Value               : out Object_Container_Base_Type'Class;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Child_Blob : Safir.Dob.Typesystem.Blob_T := null;
      Child_Is_Null : Boolean := True;
      L_Is_Null : C.char;
      L_Is_Changed : C.char;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Object_Member
        (Blob, Member, Array_Index, Child_Blob, L_Is_Null, L_Is_Changed);

      Value.Set_Changed_Here (C.char'Pos (L_Is_Changed) /= 0);

      Child_Is_Null := C.char'Pos (L_Is_Null) /= 0;

      if Child_Is_Null then
         Value.Reset_Object_Pointer;
      else
         declare
            Smart_Ptr : aliased Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                          Safir.Dob.Typesystem.Object.Factory.Create_Object (Child_Blob);
         begin
            Value.Set_Object_Pointer (Smart_Ptr'Access);
         end;
      end if;
   end Get;

   procedure Set (Value               : in Binary_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Beginning_Of_Unused : in out Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      Binary_Start : Safir.Dob.Typesystem.Blob_T;
      Binary_Size  : Safir.Dob.Typesystem.Int_32;
   begin
      if not Value.Is_Null then
         Binary_Start := Beginning_Of_Unused;
         Binary_Size := Safir.Dob.Typesystem.Int_32 (Value.Get_Val.Length);
         Safir.Dob.Typesystem.Kernel.Create_Binary_Member
           (Blob,
            Binary_Size,
            Member,
            Array_Index,
            C.char'Val (Boolean'Pos (Value.Is_Changed)),
            Beginning_Of_Unused);
         To_Int_Ptr (Binary_Start).all := Binary_Size;
         if Binary_Size > 0 then
            Binary_Start := Binary_Start + 4;  -- Adjust for the initial length Int
            for I in Value.Get_Val.First_Index .. Value.Get_Val.Last_Index loop
               Binary_Start.all := To_Char (Value.Get_Val.Element (I));
               Binary_Start := Binary_Start + 1;
            end loop;
         end if;
      elsif Value.Is_Changed then
         Safir.Dob.Typesystem.Kernel.Set_Null_Member (Blob, Member, Array_Index);
      end if;

   end Set;

   procedure Get (Value               : out Binary_Container.Container;
                  Blob                : in Safir.Dob.Typesystem.Blob_T;
                  Member              : in Safir.Dob.Typesystem.Member_Index;
                  Array_Index         : in Safir.Dob.Typesystem.Array_Index) is
      L_Val : Safir.Dob.Typesystem.Binary_Vectors.Vector;
      L_Is_Null : Boolean;
      L_Is_Changed : Boolean;
   begin
      Get (Blob, Member, Array_Index, L_Val, L_Is_Null, L_Is_Changed);
      Value := Binary_Container.Create (L_Val, L_Is_Null, L_Is_Changed);
   end Get;

   function Get_Initial_Size (Type_Id : in Safir.Dob.Typesystem.Type_Id)
                                 return Safir.Dob.Typesystem.Int_32 is
   begin
      return Safir.Dob.Typesystem.Kernel.Get_Initial_Size (Type_Id);
   end Get_Initial_Size;

end Safir.Dob.Typesystem.Blob_Operations;
