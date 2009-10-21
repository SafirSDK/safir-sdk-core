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
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Safir.Dob.Typesystem.Utilities;
pragma Elaborate_All (Safir.Dob.Typesystem.Utilities);
pragma Warnings ("H");  -- turn off warnings for hiding variable

-- Package containing a type that represents the identity of a handler.
package Safir.Dob.Typesystem.Handler_Id is

   type Handler_Id_Type is private;

   -- Constant representing default handler
   Default_Handler : constant Handler_Id_Type;

   -- Constant representing all handlers
   All_Handlers : constant Handler_Id_Type;

   -- Constructor
   --
   -- Creates a handler id from the given string.
   --
   -- Id_Str - String identifying the handler.
   --
   function Create_Handler_Id (Id_Str : in Unbounded_Wide_String) return Handler_Id_Type;

   -- Constructor
   --
   -- Creates a handler id from the given id.
   --
   -- Id - Identifier identifying the handler.
   --
   function Create_Handler_Id (Id : in Safir.Dob.Typesystem.Int_64) return Handler_Id_Type;

   -- Constructor
   --
   -- Creates a handler id from the given data.
   --
   -- Id - Identifier identifying the handler.
   -- Id_Str - String identifying the handler.
   --
   function Create_Handler_Id (Id     : in Safir.Dob.Typesystem.Int_64;
                             Id_Str : in Unbounded_Wide_String) return Handler_Id_Type;

   -- Remove the included string from the handler id.
   --
   -- This is meant to be used when this type is used as a member of a Dob object.
   -- Using this call before the object gets serialized to binary or xml (i.e.
   -- also before sending it anywhere) means that the string will not be included
   -- when the object is sent.
   --
   procedure Remove_String (Self : in out Handler_Id_Type);

   -- Equality operator
   --
   function "=" (L, R : Handler_Id_Type) return Boolean;

   -- Less-than operator.
   -- This is provided to allow HandlerIds to be stored in containers.
   --
   function "<" (L, R : Handler_Id_Type) return Boolean;

   -- Return a string representation of the handler id.
   --
   function To_String (Self : in Handler_Id_Type) return Unbounded_Wide_String;

   ----------------------------------------
   -- Operations used internally by the Dob
   ----------------------------------------

   function Get_Raw_Value (Self : in Handler_Id_Type) return Safir.Dob.Typesystem.Int_64;

   function Get_Raw_String (Self  : in Handler_Id_Type) return Unbounded_Wide_String;

   -- Get the length of the string when converted to UTF-8 encoding.
   -- Includes one byte for a null termination.
   --
   -- Returns: The length of the string of the id when converted to UTF-8
   --
   function Utf_8_String_Length (Self : in Handler_Id_Type)
                                 return Safir.Dob.Typesystem.Int_32;


   -- Convert the string to UTF-8.
   --
   -- Returns an empty string if there is no string.
   --
   -- Returns: UTF-8 representation of the string.
   --
   function Utf_8_String (Self : in Handler_Id_Type) return Unbounded_String;

private

   use Safir.Dob.Typesystem.Utilities;

   Default_Handler_Id : constant Safir.Dob.Typesystem.Int_64 :=
                          Generate_64_Bit_Hash ("DEFAULT_HANDLER");

   Default_Handler_String : constant Unbounded_Wide_String :=
                              To_Unbounded_Wide_String ("DEFAULT_HANDLER");

   All_Handlers_Id : constant Safir.Dob.Typesystem.Int_64 :=
                       Generate_64_Bit_Hash ("ALL_HANDLERS");

   All_Handlers_String : constant Unbounded_Wide_String :=
                           To_Unbounded_Wide_String ("ALL_HANDLERS");

   type Handler_Id_Type is
      record
         Id                  : Safir.Dob.Typesystem.Int_64 := Default_Handler_Id;
         Id_String           : Unbounded_Wide_String := Default_Handler_String;
         Cached_Utf_8_String : Unbounded_String := Null_Unbounded_String;
      end record;

   Default_Handler : constant Handler_Id_Type :=
                       (Id                  => Default_Handler_Id,
                        Id_String           => Default_Handler_String,
                        Cached_Utf_8_String => Null_Unbounded_String);

   All_Handlers : constant Handler_Id_Type :=
                    (Id                  => All_Handlers_Id,
                     Id_String           => All_Handlers_String,
                     Cached_Utf_8_String => Null_Unbounded_String);

end Safir.Dob.Typesystem.Handler_Id;
