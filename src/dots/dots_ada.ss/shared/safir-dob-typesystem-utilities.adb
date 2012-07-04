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
with Interfaces.C.Strings;
with Safir.Dob.Typesystem.Kernel;

pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Utilities is

   package C renames Interfaces.C;
   use type C.Strings.chars_ptr;

   type UInt is mod 2 ** 32;

   function To_Char is new Ada.Unchecked_Conversion (Safir.Dob.Typesystem.Int_8, C.char);
   function To_Int_8 is new Ada.Unchecked_Conversion (C.char, Safir.Dob.Typesystem.Int_8);

   --------------
   -- ToString --
   --------------

   function From_Utf_8 (Utf_8 : in String) return Unbounded_Wide_String
   is
      Tmp : Wide_String (1 .. Utf_8'Length); --  Worst case
      Last : Natural := 0;
      Ix : Integer := Utf_8'First;

      function Get_Char return Wide_Character;
      function Get_Char return Wide_Character
      is
         Pos : UInt := Character'Pos (Utf_8 (Ix));
         Len : Integer;
      begin
         if Pos < 128 then
            Len := 1;
         elsif (Pos and 2#1110_0000#) = 2#1100_0000# then
            Len := 2;
            Pos := Pos and 2#0001_1111#;
         elsif (Pos and 2#1111_0000#) = 2#1110_0000# then
            Len := 3;
            Pos := Pos and 2#0000_1111#;
         else
            Len := 4;
            Pos := Pos and 2#0000_0111#;
         end if;

         Ix := Ix + 1;

         for J in 2 .. Len loop
            Pos := Pos * 2 ** 6 + (Character'Pos (Utf_8 (Ix)) and 2#0011_1111#);
            Ix := Ix + 1;
         end loop;

         if Pos > Wide_Character'Pos (Wide_Character'Last) then
--              Put_Line ("Too big value: "& UInt'Image (Pos));
--              Put_Line ("Length:" & Integer'Image (Len));
--              Put ("First:" & Integer'Image (Character'Pos (S (Ix - Len))));
--              New_Line;
            --  Pos := 16#FFFD#; -- Inverse '?'
            return '#';
         end if;

         return Wide_Character'Val (Pos);
      end Get_Char;

   begin
      while Ix <= Utf_8'Last loop
         Last := Last + 1;
         Tmp (Last) := Get_Char;
      end loop;

      return To_Unbounded_Wide_String (Tmp (1 .. Last));
   end From_Utf_8;

   --------------
   -- To_Utf_8 --
   --------------

   function To_Utf_8 (W : in Unbounded_Wide_String) return String
   is
   begin
      return To_String (To_Utf_8 (W));
   end To_Utf_8;

   function To_Utf_8 (W : in Unbounded_Wide_String) return Unbounded_String is
      Tmp : Unbounded_String := To_Unbounded_String (Length (W) * 4);
      Last : Natural := 0;

      procedure Put_Char (Pos : in UInt);
      procedure Put_Char (Pos : in UInt)
      is
      begin
         if Pos < 16#80# then
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (Pos));
         elsif Pos < 16#800# then
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1100_0000# + (Pos / 2 ** 6)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + (Pos and 2#0011_1111#)));
         elsif Pos < 16#10000# then
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1110_0000# + (Pos / 2 ** 12)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + ((Pos / 2 ** 6) and 2#0011_1111#)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + (Pos and 2#0011_1111#)));
         else
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1111_0000# + (Pos / 2 ** 18)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + ((Pos / 2 ** 12) and 2#0011_1111#)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + ((Pos / 2 ** 6) and 2#0011_1111#)));
            Last := Last + 1;
            Replace_Element (Tmp, Last, Character'Val (2#1000_0000# + (Pos and 2#0011_1111#)));
         end if;

      end Put_Char;
   begin
      for J in 1 .. Length (W) loop
         Put_Char (UInt (Wide_Character'Pos (Element (W, J))));
      end loop;

      --  Put_Line ("UTF8 returns : """ & Tmp (1 .. Last) & '"');

      return Unbounded_Slice (Tmp, 1, Last);
   end To_Utf_8;

   function C_Malloc (Size : C.size_t) return Safir.Dob.Typesystem.Char_Star;
   pragma Import (C, C_Malloc, "malloc");

   procedure C_Free (X : in out Safir.Dob.Typesystem.Char_Star) is
      procedure Free (X : Safir.Dob.Typesystem.Char_Star);
      pragma Import (C, Free, "free");
   begin
      Free (X);
      X := null;
   end C_Free;

   -----------------------
   -- Binary_To_Base_64 --
   -----------------------

   function Binary_To_Base_64 (Value : Safir.Dob.Typesystem.Binary_Vectors.Vector)
                            return String is
      use type C.size_t;
      use type Ada.Containers.Count_Type;

      Dest_Buf_Size : constant Safir.Dob.Typesystem.Int_32 :=
                        Safir.Dob.Typesystem.Kernel.Calculate_Base_64_Buffer_Size
                          (Safir.Dob.Typesystem.Int_32 (Value.Length));
      Dest_Ptr : Safir.Dob.Typesystem.Char_Star := C_Malloc (C.size_t (Dest_Buf_Size));

      Source_Buf_Size : constant Safir.Dob.Typesystem.Int_32 :=
                          Safir.Dob.Typesystem.Int_32 (Value.Length);
      Source_Ptr : Safir.Dob.Typesystem.Char_Star := C_Malloc (C.size_t (Source_Buf_Size));

      Tmp_Source_Ptr : Safir.Dob.Typesystem.Char_Star;

      Dummy : Safir.Dob.Typesystem.Int_32 := 0;
   begin
      Tmp_Source_Ptr := Source_Ptr;
      for I in 0 .. Value.Length - 1 loop
         Tmp_Source_Ptr.all := To_Char (Value.Element (Safir.Dob.Typesystem.Int_32 (I)));
         Char_Ptrs.Increment (Tmp_Source_Ptr);
      end loop;

      Safir.Dob.Typesystem.Kernel.Binary_To_Base_64
        (Dest_Ptr, Dest_Buf_Size, Source_Ptr, Source_Buf_Size, Dummy);

      declare
         Str : constant String := To_Ada (Dest_Ptr, Dest_Buf_Size);
      begin
         C_Free (Dest_Ptr);
         C_Free (Source_Ptr);
         return Str;
      end;

   end Binary_To_Base_64;


   -----------------------
   -- Base_64_To_Binary --
   -----------------------
   function Base_64_To_Binary (Value : String)
                            return Safir.Dob.Typesystem.Binary_Vectors.Vector is
      use type C.size_t;

      Dest_Buf_Size : constant Safir.Dob.Typesystem.Int_32 :=
                        Safir.Dob.Typesystem.Kernel.Calculate_Binary_Buffer_Size
                          (Safir.Dob.Typesystem.Int_32 (Value'Length));
      Dest_Ptr : Safir.Dob.Typesystem.Char_Star := C_Malloc (C.size_t (Dest_Buf_Size));

      Result : Safir.Dob.Typesystem.Binary_Vectors.Vector;

      Tmp_Dest_Ptr : Safir.Dob.Typesystem.Char_Star;

      Dummy : Safir.Dob.Typesystem.Int_32 := 0;

   begin

      if Dest_Buf_Size /= 0 then
         Safir.Dob.Typesystem.Kernel.Base_64_To_Binary
           (Dest_Ptr, Dest_Buf_Size, C.To_C (Value, False), Value'Length, Dummy);

         Result.Reserve_Capacity (Ada.Containers.Count_Type (Dest_Buf_Size));
         Tmp_Dest_Ptr := Dest_Ptr;
         for I in 0 .. Dest_Buf_Size - 1 loop
            Result.Append (To_Int_8 (Tmp_Dest_Ptr.all));
            Char_Ptrs.Increment (Tmp_Dest_Ptr);
         end loop;
      end if;

      C_Free (Dest_Ptr);

      return Result;
   end Base_64_To_Binary;


   function Generate_64_Bit_Hash (Id : in Unbounded_Wide_String) return Int_64 is
      use Interfaces.C;
      function Internal (Name : in char_array) return Int_64;
      pragma Import (C, Internal, "DotsId_Generate64");
   begin
      return Internal (To_C (To_Utf_8 (Id)));
   end Generate_64_Bit_Hash;

   function Generate_64_Bit_Hash (Id : in String) return Int_64
   is
      use Interfaces.C;
      function Internal (Name : in char_array) return Int_64;
      pragma Import (C, Internal, "DotsId_Generate64");
   begin
      return Internal (To_C (Id));
   end Generate_64_Bit_Hash;

   function To_Ada (Char_Ptr : in Safir.Dob.Typesystem.Char_Star;
                    Size     : in Safir.Dob.Typesystem.Int_32) return String is
      use type C.size_t;
      Str : String (1 .. Integer (Size));
      Tmp_Ptr : Safir.Dob.Typesystem.Char_Star := Char_Ptr;
   begin
      for I in Str'Range loop
         Str (I) := C.To_Ada (Tmp_Ptr.all);
         Char_Ptrs.Increment (Tmp_Ptr);

      end loop;
      return Str;
   end To_Ada;

   procedure Copy (Destination : in Safir.Dob.Typesystem.Char_Star;
                   Source      : in C.char_array) is
      use type C.size_t;
      Tmp_Ptr : Safir.Dob.Typesystem.Char_Star := Destination;
   begin
      for I in Source'Range loop
         Tmp_Ptr.all := Source (I);
         Char_Ptrs.Increment (Tmp_Ptr);
      end loop;
   end Copy;

end Safir.Dob.Typesystem.Utilities;
