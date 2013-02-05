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
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
--  with Safir.Dob.Typesystem.Operations; use Safir.Dob.Typesystem.Operations;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Object.Factory;
with Safir.Dob.Typesystem.Kernel;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Parameters is

   package C renames Interfaces.C;
   use type C.Strings.chars_ptr;

   function To_Int_8 is new Ada.Unchecked_Conversion (C.char, Safir.Dob.Typesystem.Int_8);

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   -----------------------------
   -- GetNumberOfParameters --
   -----------------------------

   function Get_Number_Of_Parameters
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32 is
   begin
      return Safir.Dob.Typesystem.Kernel.Get_Number_Of_Parameters (Type_Id);
   end Get_Number_Of_Parameters;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (Type_Id       : Safir.Dob.Typesystem.Type_Id;
      Parameter_Name : Unbounded_Wide_String)
      return Safir.Dob.Typesystem.Parameter_Index is

      Result : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_Parameter_Id
        (Type_Id, C.To_C (To_Utf_8 (Parameter_Name)));
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type or parameter defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id)
                & " - " & To_Utf_8 (Parameter_Name) & ")");
      end if;
      return Result;
   end Get_Index;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index)
      return Unbounded_Wide_String is

      L_Val : C.Strings.chars_ptr;
   begin
      L_Val := Safir.Dob.Typesystem.Kernel.Get_Parameter_Name (Type_Id, Parameter);
      return From_Utf_8 (C.To_Ada (C.Strings.Value (L_Val)));
   end Get_Name;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index)
      return Safir.Dob.Typesystem.Member_Type is
   begin
      return Safir.Dob.Typesystem.Kernel.Get_Parameter_Type (Type_Id, Parameter);
   end Get_Type;

   -------------------
   -- Get_Type_Name --
   -------------------

   function Get_Type_Name
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index)
      return Unbounded_Wide_String is

      L_Val : C.Strings.chars_ptr;
   begin
      L_Val := Safir.Dob.Typesystem.Kernel.Get_Parameter_Type_Name (Type_Id, Parameter);
      return From_Utf_8 (C.To_Ada (C.Strings.Value (L_Val)));
   end Get_Type_Name;

   --------------------
   -- Get_Array_Size --
   --------------------
   function Get_Array_Size
     (Type_Id   : in Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index) return Safir.Dob.Typesystem.Int_32 is
   begin
      return Safir.Dob.Typesystem.Kernel.Get_Parameter_Array_Size (Type_Id, Parameter);
   end Get_Array_Size;

   -----------------
   -- Get_Boolean --
   -----------------

   function Get_Boolean
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Boolean is

      L_Value : C.char;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Boolean_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return C.char'Pos (L_Value) /= 0;
   end Get_Boolean;

   ----------------
   -- Get_Int_32 --
   ----------------

   function Get_Int_32
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Int_32 is

      L_Value : Safir.Dob.Typesystem.Int_32;

   begin
      Safir.Dob.Typesystem.Kernel.Get_Int_32_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return L_Value;
   end Get_Int_32;

   ----------------
   -- Get_Int_64 --
   ----------------

   function Get_Int_64
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Int_64 is

      L_Value : Safir.Dob.Typesystem.Int_64;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Int_64_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return L_Value;
   end Get_Int_64;

   ------------------
   -- Get_Float_32 --
   ------------------
   function Get_Float_32
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Float_32 is

      L_Value : Safir.Dob.Typesystem.Float_32;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Float_32_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return L_Value;
   end Get_Float_32;

   ------------------
   -- Get_Float_64 --
   ------------------
   function Get_Float_64
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Float_64 is

      L_Value : Safir.Dob.Typesystem.Float_64;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Float_64_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return L_Value;
   end Get_Float_64;

   ---------------------
   -- Get_Enumeration --
   ---------------------

   function Get_Enumeration
     (Type_Id       : Safir.Dob.Typesystem.Type_Id;
      Parameter     : Safir.Dob.Typesystem.Parameter_Index;
      Index         : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Int_32 is

      L_Value : Safir.Dob.Typesystem.Int_32;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Enumeration_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return L_Value;
   end Get_Enumeration;

   ---------------------
   -- Get_Instance_Id --
   ---------------------

   function Get_Instance_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index)
      return Safir.Dob.Typesystem.Instance_Id.Instance_Id_Type is

      L_Value : Safir.Dob.Typesystem.Int_64;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Parameter
        (Type_Id, Parameter, Index, L_Value, L_Str_Val);

      if L_Str_Val /= C.Strings.Null_Ptr then
         return Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
           (L_Value,
            From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val))));
      else
         return Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (L_Value);
      end if;
   end Get_Instance_Id;

   -------------------
   -- Get_Entity_Id --
   -------------------

   function Get_Entity_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index)
      return Safir.Dob.Typesystem.Entity_Id.Entity_Id_Type is

      L_Value : Safir.Dob.Typesystem.Kernel.Underlying_Entity_Id_Type;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Entity_Id_Parameter
        (Type_Id, Parameter, Index, L_Value, L_Str_Val);
      if L_Str_Val /= C.Strings.Null_Ptr then
         return Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
           (L_Value.Type_Id,
            Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id
              (L_Value.Instance, From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val)))));
      else
         return  Safir.Dob.Typesystem.Entity_Id.Create_Entity_Id
           (L_Value.Type_Id,
            Safir.Dob.Typesystem.Instance_Id.Create_Instance_Id (L_Value.Instance));
      end if;
   end Get_Entity_Id;

   ---------------------
   -- Get_Channel_Id --
   ---------------------

   function Get_Channel_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Channel_Id.Channel_Id_Type is

      L_Value : Safir.Dob.Typesystem.Int_64;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Parameter
        (Type_Id, Parameter, Index, L_Value, L_Str_Val);

      if L_Str_Val /= C.Strings.Null_Ptr then
         return Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id
           (L_Value,
            From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val))));
      else
         return Safir.Dob.Typesystem.Channel_Id.Create_Channel_Id (L_Value);
      end if;
   end Get_Channel_Id;

   function Get_Handler_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Handler_Id.Handler_Id_Type is

      L_Value : Safir.Dob.Typesystem.Int_64;
      L_Str_Val : C.Strings.chars_ptr;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Hashed_Id_Parameter
        (Type_Id, Parameter, Index, L_Value, L_Str_Val);

      if L_Str_Val /= C.Strings.Null_Ptr then
         return Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id
           (L_Value,
            From_Utf_8 (C.To_Ada (C.Strings.Value (L_Str_Val))));
      else
         return Safir.Dob.Typesystem.Handler_Id.Create_Handler_Id (L_Value);
      end if;
   end Get_Handler_Id;

   function Get_Type_Id
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Type_Id is

      L_Value : Safir.Dob.Typesystem.Type_Id;

   begin
      Safir.Dob.Typesystem.Kernel.Get_Type_Id_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return L_Value;
   end Get_Type_Id;

   ----------------
   -- Get_Object --
   ----------------

   function Get_Object
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index)
      return Safir.Dob.Typesystem.Object.Smart_Pointer'Class is

      L_Value : Safir.Dob.Typesystem.Blob_T;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Object_Parameter
        (Type_Id, Parameter, Index, L_Value);

      declare
         Object_Ptr : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                        Safir.Dob.Typesystem.Object.Factory.Create_Object (L_Value);
      begin
         Object_Ptr.Ref.Set_Changed (False);
         return Object_Ptr;
      end;
   end Get_Object;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (Type_Id    : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index       : Safir.Dob.Typesystem.Array_Index) return Unbounded_Wide_String is

      L_Value       : C.Strings.chars_ptr;
   begin
      Safir.Dob.Typesystem.Kernel.Get_String_Parameter
        (Type_Id, Parameter, Index, L_Value);
      return From_Utf_8 (C.To_Ada (C.Strings.Value (L_Value)));
   end Get_String;

   function Get_Binary
     (Type_Id   : Safir.Dob.Typesystem.Type_Id;
      Parameter : Safir.Dob.Typesystem.Parameter_Index;
      Index     : Safir.Dob.Typesystem.Array_Index) return Safir.Dob.Typesystem.Binary_Vectors.Vector is

      L_Value       : Safir.Dob.Typesystem.Char_Ptrs.Pointer;
      L_Size        : Safir.Dob.Typesystem.Int_32;
      Value         : Safir.Dob.Typesystem.Binary_Vectors.Vector;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Binary_Parameter
        (Type_Id, Parameter, Index, L_Value, L_Size);
      Value.Reserve_Capacity (Ada.Containers.Count_Type (L_Size));
      for I in 1 .. L_Size loop
         Value.Append (To_Int_8 (L_Value.all));
         Safir.Dob.Typesystem.Char_Ptrs.Increment (L_Value);
      end loop;
      return Value;
   end Get_Binary;
end Safir.Dob.Typesystem.Parameters;

