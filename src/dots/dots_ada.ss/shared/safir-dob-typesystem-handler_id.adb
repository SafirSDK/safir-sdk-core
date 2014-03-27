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
with System.Address_To_Access_Conversions;
pragma Warnings ("D");  -- turn off warnings for implicit dereference

package body Safir.Dob.Typesystem.Handler_Id is

   package Conv is new System.Address_To_Access_Conversions (Handler_Id_Type);

   procedure Throw (E : Ada.Exceptions.Exception_Id; Message : String := "")
                    renames  Ada.Exceptions.Raise_Exception;


   function Create_Handler_Id (Id : in Safir.Dob.Typesystem.Int_64) return Handler_Id_Type is
   begin
      return Handler_Id_Type'(Id           => Id,
                              Id_String    => Null_Unbounded_Wide_String,
                              Cached_Utf_8_String => Null_Unbounded_String);
   end Create_Handler_Id;

   function Create_Handler_Id (Id_Str : in Unbounded_Wide_String) return Handler_Id_Type is
      use Interfaces.C;
      function Internal (Name : in char_array) return Safir.Dob.Typesystem.Int_64;
      pragma Import (C, Internal, "LlufId_Generate64");
   begin
      if Length (Id_Str) = 0 then
         Throw (Software_Violation_Exception'Identity,
                "HandlerId can not be generated from null/empty string");
      end if;
      return Handler_Id_Type'(Id        => Internal (To_C (To_Utf_8 (Id_Str))),
                              Id_String => Id_Str,
                              Cached_Utf_8_String => Null_Unbounded_String);
   end Create_Handler_Id;


   function Create_Handler_Id (Id     : in Safir.Dob.Typesystem.Int_64;
                               Id_Str : in Unbounded_Wide_String) return Handler_Id_Type is
   begin
      return Handler_Id_Type'(Id        => Id,
                              Id_String => Id_Str,
                              Cached_Utf_8_String => Null_Unbounded_String);
   end Create_Handler_Id;


   procedure Remove_String (Self   : in out Handler_Id_Type) is
   begin
      Self.Id_String := Null_Unbounded_Wide_String;
      Self.Cached_Utf_8_String := Null_Unbounded_String;
   end Remove_String;


   function "=" (L, R : Handler_Id_Type) return Boolean is
   begin
      return L.Id = R.Id;
   end "=";

   function "<" (L, R : Handler_Id_Type) return Boolean is
   begin
      return L.Id < R.Id;
   end "<";


   function To_String (Self : in Handler_Id_Type) return Unbounded_Wide_String is
   begin
      if Length (Self.Id_String) > 0 then
         return Self.Id_String;
      elsif Self.Id = Default_Handler.Id then
         return Default_Handler.Id_String;
      elsif Self.Id = All_Handlers.Id then
         return All_Handlers.Id_String;
      else
         return Trim (To_Unbounded_Wide_String (Safir.Dob.Typesystem.Int_64'Wide_Image (Self.Id)),
                      Ada.Strings.Both);
      end if;
   end To_String;


   function Get_Raw_Value (Self : in Handler_Id_Type) return Safir.Dob.Typesystem.Int_64 is
   begin
      return Self.Id;
   end Get_Raw_Value;


   function Get_Raw_String (Self : in Handler_Id_Type) return Unbounded_Wide_String is
   begin
      return Self.Id_String;
   end Get_Raw_String;

   function Utf_8_String_Length (Self : in Handler_Id_Type)
                                 return Safir.Dob.Typesystem.Int_32 is
      Self_Ptr : constant Conv.Object_Pointer := Conv.To_Pointer (Self'Address);
   begin
      if Length (Self.Id_String) = 0 then
         return 0;
      end if;

      if Length (Self.Cached_Utf_8_String) = 0 then
         Self_Ptr.Cached_Utf_8_String :=
           To_Unbounded_String (To_Utf_8 (Self.Id_String));
      end if;

      return Safir.Dob.Typesystem.Int_32 (Length (Self.Cached_Utf_8_String) + 1);
   end Utf_8_String_Length;


   function Utf_8_String (Self : in Handler_Id_Type) return Unbounded_String is
      Self_Ptr : constant Conv.Object_Pointer := Conv.To_Pointer (Self'Address);
   begin
      if Length (Self.Id_String) /= 0 and Length (Self.Cached_Utf_8_String) = 0 then
         Self_Ptr.Cached_Utf_8_String :=
           To_Unbounded_String (To_Utf_8 (Self.Id_String));
      end if;

      return Self.Cached_Utf_8_String;
   end Utf_8_String;

end Safir.Dob.Typesystem.Handler_Id;
