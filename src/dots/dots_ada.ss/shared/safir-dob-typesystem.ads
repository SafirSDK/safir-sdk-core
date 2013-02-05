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

with Ada.Containers.Vectors;
with Interfaces.C;
with Interfaces.C.Pointers;

package Safir.Dob.Typesystem is
   pragma Preelaborate (Safir.Dob.Typesystem);

   Configuration_Error_Exception : exception;
   Illegal_Value_Exception       : exception;
   Incompatible_Types_Exception  : exception;
   Null_Exception                : exception;
   Software_Violation_Exception  : exception;
   Read_Only_Exception           : exception;

   package C renames Interfaces.C;

   type C_Unconstrained_Char_Array is array (C.size_t range <>) of aliased C.char;
   pragma Convention (C, C_Unconstrained_Char_Array);
   subtype C_Char_Array_T is C_Unconstrained_Char_Array (0 .. C.size_t'Last);

   package Char_Ptrs is
     new C.Pointers (Index              => C.size_t,
                     Element            => C.char,
                     Element_Array      => C_Unconstrained_Char_Array,
                     Default_Terminator => C.nul);

   subtype Char_Star is Char_Ptrs.Pointer;

   subtype Blob_T is Char_Star;

   type Float_32 is new Standard.Float;
   for Float_32'Size use 32;
   --  32 bits, approx 6 decimal digits

   type Float_64 is new Standard.Long_Float;
   --  64 bits, approx 15 decimal digits

   type Int_32 is range -2 ** 31 .. 2 ** 31 - 1;
   for Int_32'Size use 32;
   subtype Index is Int_32 range 0 .. Int_32'Last;
   type Int_Array is array (C.size_t range <>) of aliased Int_32;
   package Int_Ptrs is new C.Pointers (Index              => C.size_t,
                                       Element            => Int_32,
                                       Element_Array      => Int_Array,
                                       Default_Terminator => -1);  -- dummy

   type Int_8 is range -2 ** 7 .. 2 ** 7 - 1;
   for Int_8'Size use 8;
   package Binary_Vectors is new Ada.Containers.Vectors (Index, Int_8, "=");

   type Int_64 is range -2 ** 63 .. 2 ** 63 - 1;

   subtype Type_Id is Int_64;

   package Type_Id_Vectors is new Ada.Containers.Vectors (Index, Type_Id, "=");


   subtype Member_Index is Index;
   subtype Array_Index is Index;
   subtype Parameter_Index is Index;
   subtype Enumeration_Value is Index;

   type Member_Type is
     (Boolean_Member_Type,
      Enumeration_Member_Type,
      Int32_Member_Type,
      Int64_Member_Type,
      Float32_Member_Type,
      Float64_Member_Type,
      Type_Id_Member_Type,
      Instance_Id_Member_Type,
      Entity_Id_Member_Type,
      Channel_Id_Member_Type,
      Handler_Id_Member_Type,
      String_Member_Type,
      Object_Member_Type,
      Binary_Member_Type,

      --  SI32 Types
      Ampere_32_Member_Type,
      Cubic_Meter_32_Member_Type,
      Hertz_32_Member_Type,
      Joule_32_Member_Type,
      Kelvin_32_Member_Type,
      Kilogram_32_Member_Type,
      Meter_32_Member_Type,
      Meter_Per_Second_32_Member_Type,
      Meter_Per_Second_Squared_32_Member_Type,
      Newton_32_Member_Type,
      Pascal_32_Member_Type,
      Radian_32_Member_Type,
      Radian_Per_Second_32_Member_Type,
      Radian_Per_Second_Squared_32_Member_Type,
      Second_32_Member_Type,
      Square_Meter_32_Member_Type,
      Steradian_32_Member_Type,
      Volt_32_Member_Type,
      Watt_32_Member_Type,

      --  SI Long Types
      Ampere_64_Member_Type,
      Cubic_Meter_64_Member_Type,
      Hertz_64_Member_Type,
      Joule_64_Member_Type,
      Kelvin_64_Member_Type,
      Kilogram_64_Member_Type,
      Meter_64_Member_Type,
      Meter_Per_Second_64_Member_Type,
      Meter_Per_Second_Squared_64_Member_Type,
      Newton_64_Member_Type,
      Pascal_64_Member_Type,
      Radian_64_Member_Type,
      Radian_Per_Second_64_Member_Type,
      Radian_Per_Second_Squared_64_Member_Type,
      Second_64_Member_Type,
      Square_Meter_64_Member_Type,
      Steradian_64_Member_Type,
      Volt_64_Member_Type,
      Watt_64_Member_Type);
   for Member_Type'Size use 32;


end Safir.Dob.Typesystem;
