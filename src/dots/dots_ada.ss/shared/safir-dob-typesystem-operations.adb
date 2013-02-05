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
with Interfaces.C.Strings;
with Safir.Dob.Typesystem.Kernel;
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;

package body Safir.Dob.Typesystem.Operations is

   use type C.Strings.chars_ptr;

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;


   function Get_Number_Of_Type_Ids return Safir.Dob.Typesystem.Int_32 is
   begin
      return Safir.Dob.Typesystem.Kernel.Number_Of_Type_Ids;
   end Get_Number_Of_Type_Ids;

   function Get_Number_Of_Classes return Safir.Dob.Typesystem.Int_32 is
   begin
      return Safir.Dob.Typesystem.Kernel.Number_Of_Classes;
   end Get_Number_Of_Classes;

   function Get_Number_Of_Properties return Safir.Dob.Typesystem.Int_32 is
   begin
      return Safir.Dob.Typesystem.Kernel.Number_Of_Properties;
   end Get_Number_Of_Properties;

   function Get_Number_Of_Enumerations return Safir.Dob.Typesystem.Int_32 is
   begin
      return Safir.Dob.Typesystem.Kernel.Number_Of_Enumerations;
   end Get_Number_Of_Enumerations;

   function Get_All_Type_Ids return Safir.Dob.Typesystem.Type_Id_Vectors.Vector is
      Buf_Size : constant Safir.Dob.Typesystem.Int_32 := Get_Number_Of_Type_Ids;
      Tmp : aliased Safir.Dob.Typesystem.Kernel.Type_Id_Arr (1 .. Buf_Size);
      Ptr : constant Safir.Dob.Typesystem.Kernel.Type_Id_Ptrs.Pointer := Tmp (1)'Unchecked_Access;
      Dummy : Safir.Dob.Typesystem.Int_32;
      V : Safir.Dob.Typesystem.Type_Id_Vectors.Vector;
   begin
      Safir.Dob.Typesystem.Kernel.Get_All_Type_Ids (Ptr, Buf_Size, Dummy);
      V.Reserve_Capacity (Ada.Containers.Count_Type (Buf_Size));
      for I in Tmp'Range loop
         V.Append (Tmp (I));
      end loop;
      return V;
   end Get_All_Type_Ids;

   function Exists (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean is
   begin
      return C.char'Pos (Safir.Dob.Typesystem.Kernel.Type_Exists (Type_Id)) /= 0;
   end Exists;

   function Is_Class
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean is
   begin
      return C.char'Pos (Safir.Dob.Typesystem.Kernel.Is_Class (Type_Id)) /= 0;
   end Is_Class;

   function Is_Property
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean is
   begin
      return C.char'Pos (Safir.Dob.Typesystem.Kernel.Is_Property (Type_Id)) /= 0;
   end Is_Property;

   function Is_Enumeration
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean is
   begin
      return C.char'Pos (Safir.Dob.Typesystem.Kernel.Is_Enumeration (Type_Id)) /= 0;
   end Is_Enumeration;

   function Is_Exception
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean is
   begin
      return C.char'Pos (Safir.Dob.Typesystem.Kernel.Is_Exception (Type_Id)) /= 0;
   end Is_Exception;

   function Get_Type_Id
     (Type_Name : Unbounded_Wide_String) return Safir.Dob.Typesystem.Type_Id is
   begin
      return Safir.Dob.Typesystem.Kernel.Type_Id_From_Name (C.To_C (To_Utf_8 (Type_Name)));
   end Get_Type_Id;

   function Get_Name
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Unbounded_Wide_String is

      L_Val : C.Strings.chars_ptr;
   begin
      L_Val := Safir.Dob.Typesystem.Kernel.Get_Type_Name (Type_Id);
      if L_Val = C.Strings.Null_Ptr then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such type defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Type_Id) & ")");
      end if;
      return From_Utf_8 (C.To_Ada (C.Strings.Value (L_Val)));
   end Get_Name;

   function Get_Number_Of_Enumeration_Values
     (Enumeration_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Int_32 is

      Result : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Get_Number_Of_Enumeration_Values (Enumeration_Id);
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "No such enumeration exists ("
                & Safir.Dob.Typesystem.Type_Id'Image (Enumeration_Id) & ")");
      end if;
      return Result;
   end Get_Number_Of_Enumeration_Values;

   --------------------------------
   -- Get_Enumeration_Value_Name --
   --------------------------------
   function Get_Enumeration_Value_Name
     (Enumeration_Id : Safir.Dob.Typesystem.Type_Id;
      Value          : Safir.Dob.Typesystem.Int_32) return Unbounded_Wide_String is

      L_Val : C.Strings.chars_ptr;
   begin
      L_Val := Safir.Dob.Typesystem.Kernel.Get_Enumeration_Value_Name (Enumeration_Id, Value);
      if L_Val = C.Strings.Null_Ptr then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such enumeration or value defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Enumeration_Id)
                & " -" & Safir.Dob.Typesystem.Int_32'Image (Value) & ")");
      end if;
      return From_Utf_8 (C.To_Ada (C.Strings.Value (L_Val)));
   end Get_Enumeration_Value_Name;

   ---------------------------
   -- Get_Enumeration_Value --
   ---------------------------
   function Get_Enumeration_Value
     (Enumeration_Id        : Safir.Dob.Typesystem.Type_Id;
      Enumeration_Value_Name : Unbounded_Wide_String) return Safir.Dob.Typesystem.Enumeration_Value is

      Result           : Safir.Dob.Typesystem.Int_32;
   begin
      Result := Safir.Dob.Typesystem.Kernel.Enumeration_Value_From_Name
        (Enumeration_Id, C.To_C (To_Utf_8 (Enumeration_Value_Name)));
      if Result = -1 then
         Throw (Illegal_Value_Exception'Identity,
                "There is no such enumeration or value name defined ("
                & Safir.Dob.Typesystem.Type_Id'Image (Enumeration_Id)
                & " - " & To_Utf_8 (Enumeration_Value_Name) & ")");
      end if;
      return Result;
   end Get_Enumeration_Value;

   function Get_Enumeration_Checksum
     (Enumeration_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Type_Id is
      Checksum : Safir.Dob.Typesystem.Type_Id;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Enumeration_Checksum
        (Enumeration_Id, Checksum);
      return Checksum;
   end Get_Enumeration_Checksum;

   function Is_Of_Type
     (Type_Id : Safir.Dob.Typesystem.Type_Id;
      Of_Type_Id : Safir.Dob.Typesystem.Type_Id) return Boolean is
   begin
      return C.char'Pos (Safir.Dob.Typesystem.Kernel.Is_Of_Type (Type_Id, Of_Type_Id)) /= 0;
   end Is_Of_Type;

   function Get_Class_Tree
     (Root_Class : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Type_Id_Vectors.Vector is

      Buf_Size : constant Safir.Dob.Typesystem.Int_32 := Get_Number_Of_Type_Ids;
      Tmp : aliased Safir.Dob.Typesystem.Kernel.Type_Id_Arr (1 .. Buf_Size);
      Ptr : constant Safir.Dob.Typesystem.Kernel.Type_Id_Ptrs.Pointer := Tmp (1)'Unchecked_Access;
      Result_Size : Safir.Dob.Typesystem.Int_32;
      V : Safir.Dob.Typesystem.Type_Id_Vectors.Vector;
   begin
      Safir.Dob.Typesystem.Kernel.Get_Complete_Type (Root_Class, Ptr, Buf_Size, Result_Size);
      V.Reserve_Capacity (Ada.Containers.Count_Type (Result_Size));
      for I in Tmp'First .. Result_Size loop
         V.Append (Tmp (I));
      end loop;
      return V;
   end Get_Class_Tree;

   function Get_Parent_Type
     (Type_Id : Safir.Dob.Typesystem.Type_Id) return Safir.Dob.Typesystem.Type_Id is

   begin
      return Safir.Dob.Typesystem.Kernel.Get_Parent_Type (Type_Id);
   end Get_Parent_Type;

   function Has_Property
     (Class_Type     : Safir.Dob.Typesystem.Type_Id;
      Property_Type  : Safir.Dob.Typesystem.Type_Id)
      return Boolean is

      L_Has_Property : C.char := C.char'Val (0);
      L_Is_Inherited : C.char := C.char'Val (0);
   begin
      Safir.Dob.Typesystem.Kernel.Has_Property
        (Class_Type, Property_Type, L_Has_Property, L_Is_Inherited);
      return C.char'Pos (L_Has_Property) /= 0;
   end Has_Property;

   procedure Has_Property
     (Class_Type     : in Safir.Dob.Typesystem.Type_Id;
      Property_Type  : in Safir.Dob.Typesystem.Type_Id;
      Has_Property   : out Boolean;
      Is_Inherited   : out Boolean) is

      L_Has_Property : C.char := C.char'Val (0);
      L_Is_Inherited : C.char := C.char'Val (0);
   begin
      Safir.Dob.Typesystem.Kernel.Has_Property
        (Class_Type, Property_Type, L_Has_Property, L_Is_Inherited);
      Has_Property := C.char'Pos (L_Has_Property) /= 0;
      Is_Inherited := C.char'Pos (L_Is_Inherited) /= 0;
   end Has_Property;

end Safir.Dob.Typesystem.Operations;
