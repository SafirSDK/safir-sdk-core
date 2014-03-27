-------------------------------------------------------------------------------
--
--  Copyright Saab AB, 2009-2013 (http://safir.sourceforge.net)
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
with Safir.Dob.Typesystem.Utilities; use Safir.Dob.Typesystem.Utilities;
with Safir.Dob.Typesystem.Kernel;
with Safir.Dob.Typesystem.Object.Factory;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Serialization is

   package C renames Interfaces.C;
   use type C.Strings.chars_ptr;

--     function To_Int_8 is new Ada.Unchecked_Conversion (C.char,
--                                                        Safir.Dob.Typesystem.Int_8);

   function To_Char is new Ada.Unchecked_Conversion (Safir.Dob.Typesystem.Int_8, C.char);
   function To_Int_8 is new Ada.Unchecked_Conversion (C.char, Safir.Dob.Typesystem.Int_8);

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;

   function C_Malloc (Size : C.size_t) return Safir.Dob.Typesystem.Char_Star;
   pragma Import (C, C_Malloc, "malloc");

   procedure C_Free (X : in out Safir.Dob.Typesystem.Char_Star) is
      procedure Free (X : Safir.Dob.Typesystem.Char_Star);
      pragma Import (C, Free, "free");
   begin
      Free (X);
      X := null;
   end C_Free;

   --------------------------
   -- To_Xml (from object) --
   --------------------------
   function To_Xml
     (Object : Safir.Dob.Typesystem.Object.Smart_Pointer'Class)
      return Unbounded_Wide_String is

      use type Safir.Dob.Typesystem.Object.Object_Class_Access;

      Blob_Size : constant Safir.Dob.Typesystem.Int_32 := Object.Ref.Calculate_Blob_Size;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
   begin
      if Object.Ref = null then
         Throw (Software_Violation_Exception'Identity,
                "Attempt to serialize a null pointer to xml!");
      end if;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Kernel.Format_Blob (Blob,
                                               Blob_Size,
                                               Object.Ref.Get_Type_Id,
                                               Beginning_Of_Unused);
      Object.Ref.Write_To_Blob (Blob, Beginning_Of_Unused);

      declare
         Str : constant Unbounded_Wide_String := To_Xml (Blob);
      begin
         C_Free (Blob);
         return Str;
      end;
   end To_Xml;

   ---------------------------------
   -- To_Object (from XML string) --
   ---------------------------------
   function To_Object (Xml : in Unbounded_Wide_String) return
     Safir.Dob.Typesystem.Object.Smart_Pointer'Class is

      use type Blob_T;

      Deleter     : Safir.Dob.Typesystem.Kernel.Blob_Deleter_Cb_Type;
      pragma Convention (C, Deleter);

      Blob : Safir.Dob.Typesystem.Blob_T;
      Xml_8 : constant String := Safir.Dob.Typesystem.Utilities.To_Utf_8 (Xml);
   begin
      Safir.Dob.Typesystem.Kernel.Xml_To_Blob (Blob, Deleter, C.To_C (Xml_8));

      if Blob = null then
         Throw (Illegal_Value_Exception'Identity,
                "Something is wrong with the XML-formated object");
      end if;

      declare
         Obj_Ptr : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                     Safir.Dob.Typesystem.Object.Factory.Create_Object (Blob);
      begin
         Deleter.all (Blob);
         return Obj_Ptr;
      end;

   end To_Object;

   --------------------------
   -- To_Xml (from binary) --
   --------------------------
   function To_Xml (Binary : in Safir.Dob.Typesystem.Binary_Vectors.Vector)
                    return Unbounded_Wide_String is
      use type Blob_T;

      Blob      : Safir.Dob.Typesystem.Blob_T;
      Tmp_Ptr : Safir.Dob.Typesystem.Blob_T;
   begin
      Blob := C_Malloc (C.size_t (Binary.Length));
      Tmp_Ptr := Blob;
      for I in Binary.First_Index .. Binary.Last_Index loop
         Tmp_Ptr.all := To_Char (Binary.Element (I));
         Tmp_Ptr := Tmp_Ptr + 1;
      end loop;

      declare
         Str : constant Unbounded_Wide_String := To_Xml (Blob);
      begin
         C_Free (Blob);
         return Str;
      end;
   end To_Xml;

   ------------------------
   -- To_Xml (from blob) --
   ------------------------
   function To_Xml
     (Blob : Safir.Dob.Typesystem.Blob_T) return Unbounded_Wide_String is

      Xml_Buf_Size : Safir.Dob.Typesystem.Int_32 := 100_000;
      Xml_Buf      : Safir.Dob.Typesystem.Char_Star;
      Result_Size : Safir.Dob.Typesystem.Int_32;
   begin
      Xml_Buf := C_Malloc (C.size_t (Xml_Buf_Size));

      Safir.Dob.Typesystem.Kernel.Better_Blob_To_Xml
        (Xml_Buf, Blob, Xml_Buf_Size, Result_Size);

      if Result_Size > Xml_Buf_Size then
         Xml_Buf_Size := Result_Size;
         C_Free (Xml_Buf);
         Xml_Buf := C_Malloc (C.size_t (Xml_Buf_Size));

         Safir.Dob.Typesystem.Kernel.Better_Blob_To_Xml (Xml_Buf,
                                                         Blob,
                                                         Xml_Buf_Size,
                                                         Result_Size);
            if Result_Size /= Xml_Buf_Size then
               Throw (Software_Violation_Exception'Identity,
                      "Error in serialization buffer sizes!!!");
            end if;
      end if;

      declare
         Xml_String : constant String := To_Ada (Xml_Buf, Result_Size - 1); -- Remove null
      begin
         C_Free (Xml_Buf);
         return From_Utf_8 (Xml_String);
      end;
   end To_Xml;

   ---------------------------
   -- To_Json (from object) --
   ---------------------------
   function To_Json
     (Object : Safir.Dob.Typesystem.Object.Smart_Pointer'Class)
      return Unbounded_Wide_String is

      use type Safir.Dob.Typesystem.Object.Object_Class_Access;

      Blob_Size : constant Safir.Dob.Typesystem.Int_32 := Object.Ref.Calculate_Blob_Size;
      Blob : Safir.Dob.Typesystem.Blob_T;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
   begin
      if Object.Ref = null then
         Throw (Software_Violation_Exception'Identity,
                "Attempt to serialize a null pointer to json!");
      end if;

      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Kernel.Format_Blob (Blob,
                                               Blob_Size,
                                               Object.Ref.Get_Type_Id,
                                               Beginning_Of_Unused);
      Object.Ref.Write_To_Blob (Blob, Beginning_Of_Unused);

      declare
         Str : constant Unbounded_Wide_String := To_Json (Blob);
      begin
         C_Free (Blob);
         return Str;
      end;
   end To_Json;

   ---------------------------------------------
   -- To_Object_From_Json (from Json string) --
   ---------------------------------------------
   function To_Object_From_Json (Json : in Unbounded_Wide_String) return
     Safir.Dob.Typesystem.Object.Smart_Pointer'Class is

      use type Blob_T;

      Deleter     : Safir.Dob.Typesystem.Kernel.Blob_Deleter_Cb_Type;
      pragma Convention (C, Deleter);

      Blob : Safir.Dob.Typesystem.Blob_T;
      Json_8 : constant String := Safir.Dob.Typesystem.Utilities.To_Utf_8 (Json);
   begin
      Safir.Dob.Typesystem.Kernel.Json_To_Blob (Blob, Deleter, C.To_C (Json_8));

      if Blob = null then
         Throw (Illegal_Value_Exception'Identity,
                "Something is wrong with the JSON-formated object");
      end if;

      declare
         Obj_Ptr : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                     Safir.Dob.Typesystem.Object.Factory.Create_Object (Blob);
      begin
         Deleter.all (Blob);
         return Obj_Ptr;
      end;

   end To_Object_From_Json;

   ---------------------------
   -- To_Json (from binary) --
   ---------------------------
   function To_Json (Binary : in Safir.Dob.Typesystem.Binary_Vectors.Vector)
                    return Unbounded_Wide_String is
      use type Blob_T;

      Blob      : Safir.Dob.Typesystem.Blob_T;
      Tmp_Ptr : Safir.Dob.Typesystem.Blob_T;
   begin
      Blob := C_Malloc (C.size_t (Binary.Length));
      Tmp_Ptr := Blob;
      for I in Binary.First_Index .. Binary.Last_Index loop
         Tmp_Ptr.all := To_Char (Binary.Element (I));
         Tmp_Ptr := Tmp_Ptr + 1;
      end loop;

      declare
         Str : constant Unbounded_Wide_String := To_Json (Blob);
      begin
         C_Free (Blob);
         return Str;
      end;
   end To_Json;

   -------------------------
   -- To_Json (from blob) --
   -------------------------
   function To_Json
     (Blob : Safir.Dob.Typesystem.Blob_T) return Unbounded_Wide_String is

      Json_Buf_Size : Safir.Dob.Typesystem.Int_32 := 100_000;
      Json_Buf      : Safir.Dob.Typesystem.Char_Star;
      Result_Size : Safir.Dob.Typesystem.Int_32;
   begin
      Json_Buf := C_Malloc (C.size_t (Json_Buf_Size));

      Safir.Dob.Typesystem.Kernel.Blob_To_Json
        (Json_Buf, Blob, Json_Buf_Size, Result_Size);

      if Result_Size > Json_Buf_Size then
         Json_Buf_Size := Result_Size;
         C_Free (Json_Buf);
         Json_Buf := C_Malloc (C.size_t (Json_Buf_Size));

         Safir.Dob.Typesystem.Kernel.Blob_To_Json (Json_Buf,
                                                   Blob,
                                                   Json_Buf_Size,
                                                   Result_Size);
            if Result_Size /= Json_Buf_Size then
               Throw (Software_Violation_Exception'Identity,
                      "Error in serialization buffer sizes!!!");
            end if;
      end if;

      declare
         Json_String : constant String := To_Ada (Json_Buf, Result_Size - 1); -- Remove null
      begin
         C_Free (Json_Buf);
         return From_Utf_8 (Json_String);
      end;
   end To_Json;

   ---------------
   -- To_Binary --
   ---------------
   procedure To_Binary (Object : in Safir.Dob.Typesystem.Object.Smart_Pointer'Class;
                       Binary : out Safir.Dob.Typesystem.Binary_Vectors.Vector) is
      use type Safir.Dob.Typesystem.Object.Object_Class_Access;
      use type Blob_T;

      Blob : Safir.Dob.Typesystem.Blob_T;
      Tmp_Blob_Ptr : Safir.Dob.Typesystem.Blob_T;
      Blob_Size : Safir.Dob.Typesystem.Int_32;
      Beginning_Of_Unused : Safir.Dob.Typesystem.Blob_T;
   begin
      if Object.Ref = null then
         Throw (Software_Violation_Exception'Identity,
                "Attempt to serialize a null pointer to binary!");
      end if;

      Blob_Size := Object.Ref.Calculate_Blob_Size;


      Blob := C_Malloc (C.size_t (Blob_Size));

      Safir.Dob.Typesystem.Kernel.Format_Blob (Blob,
                                               Blob_Size,
                                               Object.Ref.Get_Type_Id,
                                               Beginning_Of_Unused);
      Object.Ref.Write_To_Blob (Blob, Beginning_Of_Unused);

      Binary.Clear;
      Binary.Reserve_Capacity (Ada.Containers.Count_Type (Blob_Size));
      Tmp_Blob_Ptr := Blob;
      for I in 1 .. Blob_Size loop
         Binary.Append (To_Int_8 (Tmp_Blob_Ptr.all));
         Tmp_Blob_Ptr := Tmp_Blob_Ptr + 1;
      end loop;

      C_Free (Blob);

   end To_Binary;

   -----------------------------
   -- To_Object (from bianry) --
   -----------------------------
   function To_Object (Binary : in Safir.Dob.Typesystem.Binary_Vectors.Vector)
                       return Safir.Dob.Typesystem.Object.Smart_Pointer'Class is
      use type Blob_T;

      Blob      : Safir.Dob.Typesystem.Blob_T;
      Tmp_Ptr : Safir.Dob.Typesystem.Blob_T;
   begin
      Blob := C_Malloc (C.size_t (Binary.Length));
      Tmp_Ptr := Blob;
      for I in Binary.First_Index .. Binary.Last_Index loop
         Tmp_Ptr.all := To_Char (Binary.Element (I));
         Tmp_Ptr := Tmp_Ptr + 1;
      end loop;

      declare
         Obj_Ptr : constant Safir.Dob.Typesystem.Object.Smart_Pointer'Class :=
                     Safir.Dob.Typesystem.Object.Factory.Create_Object (Blob);
      begin
         C_Free (Blob);
         return Obj_Ptr;
      end;
   end To_Object;

end Safir.Dob.Typesystem.Serialization;
